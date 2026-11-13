module arm64

import v.ssa

// C function wrappers for external linking, after cgo's `_cgo_` stubs.
//
// A `fn C.name(...)` binding can name something that has no symbol to link
// against: a function-like macro (`#define sapp_x(a) ...`) or a `static inline`
// function of a header (vlib/macos/objc_bridge.h). The C backend compiles those
// in place; a native backend cannot. When the program is linked with the
// system linker, the driver asks the C compiler which C functions are like
// that (see the driver's arm64_probe_c_functions), and this file writes, for
// each one, a small C function with external linkage into the program's C side.
// V code then calls `__v_cfn_<name>`.
//
// The wrapper's parameter and result types are spelled from the SSA types of
// the call: integers keep their width and signedness, pointers become
// `void *`, and structs by value become structs with the same members, which
// arm64 passes and returns exactly like the original type. The wrapped
// function is called with its own C prototype, so C converts the arguments.

// CFunctionKind says why a C function needs a wrapper.
pub enum CFunctionKind {
	macro    // a function-like macro
	internal // a function with internal linkage (`static`, `static inline`)
}

// CExternSignature is the C signature of calls to a C function, as the SSA
// module types its arguments and result.
struct CExternSignature {
	ret         ssa.TypeID
	params      []ssa.TypeID
	is_variadic bool
}

// c_extern_call_names returns the C functions that V code calls or takes the
// address of, without the ones already bound to another symbol.
pub fn (g &Gen) c_extern_call_names() []string {
	mut names := []string{}
	for function in g.m.funcs {
		if !function.is_c_extern {
			continue
		}
		name := function.name.trim_string_left('C.')
		if name.len == 0 || name in g.m.c_extern_symbols || name in names
			|| name in arm64_native_tls_symbols
			|| (g.gc_runtime && name in arm64_gc_runtime_symbols) {
			continue
		}
		names << name
	}
	return names
}

fn (g &Gen) c_extern_signatures() map[string]CExternSignature {
	mut result := map[string]CExternSignature{}
	for instr in g.m.instrs {
		if instr.op != .call || instr.operands.len == 0 {
			continue
		}
		callee := g.m.values[instr.operands[0]]
		if callee.kind != .func_ref || callee.index < 0 || callee.index >= g.m.funcs.len {
			continue
		}
		function := g.m.funcs[callee.index]
		if !function.is_c_extern {
			continue
		}
		name := function.name.trim_string_left('C.')
		if name in result {
			continue
		}
		mut params := []ssa.TypeID{}
		for arg in instr.operands[1..] {
			params << g.m.values[arg].typ
		}
		result[name] = CExternSignature{
			ret:         instr.typ
			params:      params
			is_variadic: function.is_variadic
		}
	}
	return result
}

// c_function_wrappers returns the C source of the wrappers for `kinds` and
// binds the generator's calls to them. Functions whose calls cannot be
// spelled in C (variadic ones, or ones V code never calls directly) are left
// alone; the link reports them.
pub fn (mut g Gen) c_function_wrappers(kinds map[string]CFunctionKind) string {
	if kinds.len == 0 {
		return ''
	}
	signatures := g.c_extern_signatures()
	mut types := CShimTypes{}
	mut bodies := []string{}
	mut names := kinds.keys()
	names.sort()
	for name in names {
		kind := kinds[name]
		signature := signatures[name] or { continue }
		if signature.is_variadic {
			continue
		}
		ret := types.spell(g, signature.ret)
		ret_c := g.cshim_c_type(signature.ret) or { ret }
		mut params := []string{}
		mut param_types := []string{}
		mut args := []string{}
		mut conversions := []string{}
		mut surrogate_param := false
		for i, param in signature.params {
			typ := types.spell(g, param)
			params << '${typ} a${i}'
			if cshim_is_aggregate(g.m, param) {
				if c_type := g.cshim_c_type(param) {
					// The twin has the struct's probed layout: copy it into the
					// real C type, so C passes the argument by the C ABI.
					conversions << '${c_type} p${i}; __builtin_memcpy(&p${i}, &a${i}, sizeof(p${i}));'
					param_types << c_type
					args << 'p${i}'
					continue
				}
				surrogate_param = true
			}
			param_types << typ
			args << 'a${i}'
		}
		wrapper := '__v_cfn_${name}'
		param_list := if params.len == 0 { 'void' } else { params.join(', ') }
		// Like cgo's stubs, call the function with its own prototype, so C
		// converts the arguments: a V `f64` reaches a `float` parameter as a
		// float. A struct passed by value with no C spelling (a V struct) has
		// only a layout twin, so such calls go through a pointer with the
		// twin's signature.
		callee := if kind == .macro || !surrogate_param {
			name
		} else {
			type_list := if param_types.len == 0 { 'void' } else { param_types.join(', ') }
			'((${ret_c} (*)(${type_list}))(&${name}))'
		}
		call := '${callee}(${args.join(', ')})'
		prefix := if conversions.len > 0 { conversions.join(' ') + ' ' } else { '' }
		body := if ret == 'void' {
			'${prefix}${call};'
		} else if cshim_is_aggregate(g.m, signature.ret) {
			// A struct result (`CGRectMake`) is copied into its layout twin; C
			// cannot convert between two struct types. The twin has the probed
			// size, so the whole C struct is copied, and the native caller
			// finds it where its own classification of the type puts it.
			'${prefix}${ret} r; __typeof__(${call}) v = ${call}; __builtin_memcpy(&r, &v, sizeof(r) < sizeof(v) ? sizeof(r) : sizeof(v)); return r;'
		} else {
			'${prefix}return (${ret})${call};'
		}
		bodies << '${ret} ${wrapper}(${param_list}) { ${body} }'
		g.m.c_extern_symbols[name] = wrapper
	}
	if bodies.len == 0 {
		return ''
	}
	return '\n// V arm64: wrappers for C macros and static functions called from V.\n' +
		types.decls.join('\n') + '\n' + bodies.join('\n') + '\n'
}

// cshim_c_type returns the C spelling of a C struct whose layout the driver
// probed (`struct big`, or a typedef name). The wrappers are compiled with the
// program's headers, so they can use it.
fn (g &Gen) cshim_c_type(typ ssa.TypeID) ?string {
	name := g.m.c_struct_names[int(typ)] or { return none }
	if name.len == 0 {
		return none
	}
	return name
}

fn cshim_is_aggregate(m &ssa.Module, typ ssa.TypeID) bool {
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return false
	}
	return m.type_store.types[typ].kind in [.struct_t, .array_t]
}

struct CShimTypes {
mut:
	names map[int]string
	decls []string
}

// spell returns a C type with the same layout and arm64 calling convention as
// the SSA type `typ`, declaring structs as needed.
fn (mut t CShimTypes) spell(g &Gen, typ ssa.TypeID) string {
	m := g.m
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return 'void'
	}
	info := m.type_store.types[typ]
	return match info.kind {
		.void_t {
			'void'
		}
		.int_t {
			match info.width {
				1 { '_Bool' }
				8 {
					if info.is_unsigned { 'uint8_t' } else { 'int8_t' }
				}
				16 {
					if info.is_unsigned { 'uint16_t' } else { 'int16_t' }
				}
				32 {
					if info.is_unsigned { 'uint32_t' } else { 'int32_t' }
				}
				else {
					if info.is_unsigned { 'uint64_t' } else { 'int64_t' }
				}
			}
		}
		.float_t {
			if info.width == 32 { 'float' } else { 'double' }
		}
		.ptr_t, .func_t {
			'void *'
		}
		.struct_t, .array_t {
			t.spell_aggregate(g, typ)
		}
		else {
			'void *'
		}
	}
}

// spell_aggregate declares the layout twin of an aggregate: every field at the
// offset the native code uses (the C compiler's, for a probed C struct), byte
// padding for the gaps a partial V declaration leaves, and the type's full
// size. C then classifies the twin as the native backend classifies the type
// (homogeneous float aggregate, general registers, or indirect).
fn (mut t CShimTypes) spell_aggregate(g &Gen, typ ssa.TypeID) string {
	if name := t.names[int(typ)] {
		return name
	}
	m := g.m
	info := m.type_store.types[typ]
	keyword := if info.is_union { 'union' } else { 'struct' }
	name := '${keyword} __v_cfn_t${int(typ)}'
	t.names[int(typ)] = name
	size := m.type_size(typ)
	align := m.type_align(typ)
	mut fields := []string{}
	mut packed := info.is_packed
	if info.kind == .array_t {
		elem := t.spell(g, info.elem_type)
		fields << '${elem} items[${if info.len > 0 { info.len } else { 1 }}];'
	} else if info.is_union {
		mut largest := 0
		for i, field in info.fields {
			fields << '${t.spell(g, field)} f${i};'
			largest = int_max(largest, m.type_size(field))
		}
		// A union larger than its V members, or one C would take for a float
		// aggregate while the native backend does not, gets a byte member.
		is_hfa := if _ := g.c_homogeneous_float_aggregate(typ) { true } else { false }
		if size > largest || (!is_hfa && size > 0) {
			fields << 'unsigned char f_bytes[${size}];'
		}
	} else {
		order := cshim_fields_by_offset(m, typ)
		mut cursor := 0
		for i in order {
			field := info.fields[i]
			offset := m.struct_field_offset(typ, i)
			if offset < cursor {
				// Overlapping fields, which a C struct cannot spell: the bytes
				// are already covered.
				continue
			}
			if offset > cursor {
				fields << 'unsigned char f_pad${i}[${offset - cursor}];'
			}
			field_align := m.type_align(field)
			if field_align > 1 && offset % field_align != 0 {
				packed = true
			}
			fields << '${t.spell(g, field)} f${i};'
			cursor = offset + m.type_size(field)
		}
		if size > cursor {
			fields << 'unsigned char f_tail[${size - cursor}];'
		}
	}
	attrs := if packed { ' __attribute__((packed))' } else { '' }
	align_attr := if align > 0 { ' __attribute__((aligned(${align})))' } else { '' }
	t.decls << '${name} { ${fields.join(' ')} }${attrs}${align_attr};'
	return name
}

// cshim_fields_by_offset returns the field indices of a struct in offset order;
// a probed C layout need not follow the V declaration's order.
fn cshim_fields_by_offset(m &ssa.Module, typ ssa.TypeID) []int {
	count := m.type_store.types[typ].fields.len
	mut offsets := []int{cap: count}
	for i in 0 .. count {
		offsets << m.struct_field_offset(typ, i)
	}
	mut order := []int{cap: count}
	for i in 0 .. count {
		mut at := order.len
		for at > 0 && offsets[order[at - 1]] > offsets[i] {
			at--
		}
		order.insert(at, i)
	}
	return order
}
