module wit

import wasm

pub fn wrap_component(core []u8, doc WitDoc, world string) []u8 {
	// Experimental: currently wraps the core module into a component container.
	// Canonical ABI lifting of individual functions is a follow-up.
	_ = doc
	_ = world
	return wasm.wrap_core_module(core)
}
