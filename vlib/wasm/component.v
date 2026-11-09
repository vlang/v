module wasm

import encoding.leb128

// wrap_core_module wraps a core WebAssembly module's bytes into a
// WebAssembly component. The component contains the original module as a
// core module section. This is an experimental, minimal wrapper.
pub fn wrap_core_module(core []u8) []u8 {
	mut out := []u8{}
	// preamble: magic + version (0x0d 0x00) + layer (0x01 0x00)
	out << u8(0x00)
	out << u8(0x61)
	out << u8(0x73)
	out << u8(0x6d)
	out << u8(0x0d)
	out << u8(0x00)
	out << u8(0x01)
	out << u8(0x00)
	// section 1: core module
	out << u8(1) // section id
	size_bytes := leb128.encode_u32(u32(core.len))
	out << size_bytes
	out << core
	return out
}

// component_magic returns the 8-byte component preamble.
pub fn component_preamble() []u8 {
	return [u8(0x00), 0x61, 0x73, 0x6d, 0x0d, 0x00, 0x01, 0x00]
}

pub fn is_component(bytes []u8) bool {
	if bytes.len < 8 { return false }
	return bytes[0] == 0x00 && bytes[1] == 0x61 && bytes[2] == 0x73 && bytes[3] == 0x6d
		&& bytes[4] == 0x0d && bytes[5] == 0x00 && bytes[6] == 0x01 && bytes[7] == 0x00
}
