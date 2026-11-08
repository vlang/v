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
// The wrapper's C types are spelled from the SSA types of the call: integers
// keep their width and signedness, pointers become `void *`, and structs by
// value become structs with the same members, which arm64 passes and returns
// exactly like the original type.

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
		ret := types.spell(g.m, signature.ret)
		mut params := []string{}
		mut param_types := []string{}
		mut args := []string{}
		for i, param in signature.params {
			typ := types.spell(g.m, param)
			params << '${typ} a${i}'
			param_types << typ
			args << 'a${i}'
		}
		wrapper := '__v_cfn_${name}'
		param_list := if params.len == 0 { 'void' } else { params.join(', ') }
		callee := if kind == .macro {
			name
		} else {
			type_list := if param_types.len == 0 { 'void' } else { param_types.join(', ') }
			'((${ret} (*)(${type_list}))(&${name}))'
		}
		call := '${callee}(${args.join(', ')})'
		body := if ret == 'void' { '${call};' } else { 'return (${ret})${call};' }
		bodies << '${ret} ${wrapper}(${param_list}) { ${body} }'
		g.m.c_extern_symbols[name] = wrapper
	}
	if bodies.len == 0 {
		return ''
	}
	return '\n// V arm64: wrappers for C macros and static functions called from V.\n' +
		types.decls.join('\n') + '\n' + bodies.join('\n') + '\n'
}

struct CShimTypes {
mut:
	names map[int]string
	decls []string
}

// spell returns a C type with the same layout and arm64 calling convention as
// the SSA type `typ`, declaring structs as needed.
fn (mut t CShimTypes) spell(m &ssa.Module, typ ssa.TypeID) string {
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
			t.spell_aggregate(m, typ)
		}
		else {
			'void *'
		}
	}
}

fn (mut t CShimTypes) spell_aggregate(m &ssa.Module, typ ssa.TypeID) string {
	if name := t.names[int(typ)] {
		return name
	}
	info := m.type_store.types[typ]
	keyword := if info.is_union { 'union' } else { 'struct' }
	name := '${keyword} __v_cfn_t${int(typ)}'
	t.names[int(typ)] = name
	mut fields := []string{}
	if info.kind == .array_t {
		elem := t.spell(m, info.elem_type)
		fields << '${elem} items[${if info.len > 0 { info.len } else { 1 }}];'
	} else {
		for i, field in info.fields {
			fields << '${t.spell(m, field)} f${i};'
		}
	}
	attrs := if info.is_packed { ' __attribute__((packed))' } else { '' }
	align := if info.alignment > 0 { ' __attribute__((aligned(${info.alignment})))' } else { '' }
	t.decls << '${name} { ${fields.join(' ')} }${attrs}${align};'
	return name
}
