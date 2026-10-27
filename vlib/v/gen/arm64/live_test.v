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
