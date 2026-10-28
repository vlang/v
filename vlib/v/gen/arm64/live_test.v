module arm64

import v.ssa

fn test_extract_function_returns_bytes_between_symbols() {
	mut m := ssa.Module.new()
	mut g := Gen.new(m)
	g.macho.text_data = [u8(0xAA), 0xBB, 0xCC, 0xDD, 0x11, 0x22, 0x33, 0x44]
	g.macho.add_symbol('_foo', 0, false, 1)
	g.macho.add_symbol('_bar', 4, false, 1)
	code := g.extract_function('foo')
	assert code == [u8(0xAA), 0xBB, 0xCC, 0xDD]
}

fn test_extract_function_missing_symbol_returns_empty() {
	mut m := ssa.Module.new()
	mut g := Gen.new(m)
	g.macho.text_data = [u8(0xAA)]
	g.macho.add_symbol('_foo', 0, false, 1)
	code := g.extract_function('nope')
	assert code.len == 0
}

fn test_extract_function_rejects_function_with_relocations() {
	mut m := ssa.Module.new()
	mut g := Gen.new(m)
	g.macho.text_data = [u8(0xAA), 0xBB, 0xCC, 0xDD, 0x11, 0x22, 0x33, 0x44]
	g.macho.add_symbol('_foo', 0, false, 1)
	g.macho.add_symbol('_bar', 4, false, 1)
	sym_idx := g.macho.add_undefined('_println')
	// Simulate a call inside `foo` needing a branch26 fixup.
	g.macho.add_reloc(0, sym_idx, arm64_reloc_branch26, true)
	code := g.extract_function('foo')
	assert code.len == 0
}

fn test_extract_function_rejects_direct_call_without_reloc() {
	mut m := ssa.Module.new()
	mut g := Gen.new(m)
	// `foo` calls the earlier-defined helper `bar` via a direct BL with
	// its displacement baked in (no relocation entry): BL @0 targeting 8
	// is 0x94000002, followed by RET (0xD65F03C0).
	g.macho.text_data = [u8(0x02), 0x00, 0x00, 0x94, 0xC0, 0x03, 0x5F, 0xD6, 0xC0, 0x03, 0x5F,
		0xD6]
	g.macho.add_symbol('_foo', 0, false, 1)
	g.macho.add_symbol('_bar', 8, false, 1)
	code := g.extract_function('foo')
	assert code.len == 0
}

fn test_extract_function_accepts_recursive_call() {
	mut m := ssa.Module.new()
	mut g := Gen.new(m)
	// BL to self (0x94000000): the target moves with the copy, so the
	// displacement stays valid.
	g.macho.text_data = [u8(0x00), 0x00, 0x00, 0x94]
	g.macho.add_symbol('_foo', 0, false, 1)
	code := g.extract_function('foo')
	assert code == [u8(0x00), 0x00, 0x00, 0x94]
}
