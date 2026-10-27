module arm64

// extract_function returns the raw machine code bytes of a single function,
// bounded by the next symbol in the text section. It is used for hot code
// reloading: the extracted bytes are written to a file, then loaded into an
// executable page of the running binary and called via a function pointer.
// Ported from v2 (vlib/v2_toberemoved/gen/arm64/arm64.v).
//
// Only self-contained functions are supported: if the function body needs
// address fixups at its load address (calls, string literals, globals), the
// raw bytes cannot be relocated by a plain copy, so extraction is rejected.
// Callers (notably `-hot-fn`) treat an empty result as a build error.
pub fn (mut g Gen) extract_function(fn_name string) []u8 {
	target_sym := '_' + fn_name

	// Collect text section symbol offsets, sorted
	mut sym_names := []string{}
	mut sym_values := []u64{}
	for sym in g.macho.symbols {
		if sym.sect == 1 && (sym.type_ & 0x0E) == 0x0E {
			sym_names << sym.name
			sym_values << sym.value
		}
	}
	// Bubble sort by value (small N)
	for i in 0 .. sym_values.len {
		for j in 0 .. sym_values.len - 1 - i {
			if sym_values[j] > sym_values[j + 1] {
				sym_values[j], sym_values[j + 1] = sym_values[j + 1], sym_values[j]
				sym_names[j], sym_names[j + 1] = sym_names[j + 1], sym_names[j]
			}
		}
	}

	// Find target function boundaries
	mut fn_start := u64(0)
	mut fn_end := u64(g.macho.text_data.len)
	mut found := false
	for i, name in sym_names {
		if name == target_sym {
			fn_start = sym_values[i]
			if i + 1 < sym_values.len {
				fn_end = sym_values[i + 1]
			}
			found = true
			break
		}
	}

	if !found {
		eprintln('extract_function: symbol "${target_sym}" not found')
		return []u8{}
	}

	code_size := int(fn_end - fn_start)
	if code_size <= 0 || fn_start + u64(code_size) > u64(g.macho.text_data.len) {
		eprintln('extract_function: invalid range ${fn_start}..${fn_end}')
		return []u8{}
	}

	// The sliced bytes are pre-link object code. Any relocation inside the
	// range (branch26 call, adrp/ldr literal or global access, GOT load)
	// would target the wrong address after a plain copy to a new executable
	// page, so refuse to extract such functions instead of emitting silently
	// broken machine code.
	for r in g.macho.relocs {
		if u64(r.addr) >= fn_start && u64(r.addr) < fn_end {
			eprintln('extract_function: "${target_sym}" needs address fixups (reloc type ${r.type_} at offset ${r.addr}); only self-contained functions can be hot-reloaded')
			return []u8{}
		}
	}

	return g.macho.text_data[int(fn_start)..int(fn_end)].clone()
}
