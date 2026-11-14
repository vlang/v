module c

import v.pref

// The export wrappers a `-shared` build emits carry the symbol visibility,
// and a PE linker ignores the ELF spelling of it. tcc, gcc and clang all
// accept `__declspec(dllexport)` on Windows, so every one of them must get it;
// MSVC was the only one that did, and a Windows DLL built with anything else
// silently exported nothing. Measured with a `@[export]` function linked from
// C: no Export Directory without this, the export present with it.
fn test_exported_symbol_attribute_marks_windows_shared_exports() {
	for cc in ['msvc', 'tcc', 'gcc', 'clang'] {
		mut g := FlatGen.new()
		g.is_shared = true
		g.ccompiler = cc
		g.set_target(pref.Target{ os: 'windows', pointer_bits: 64 })
		assert g.exported_symbol_attribute() == '__declspec(dllexport) ',
			'${cc} on Windows must dllexport shared wrappers'
	}
	// Anywhere else the ELF spelling is the right one, whatever the compiler.
	for target_os in ['linux', 'macos'] {
		mut g := FlatGen.new()
		g.is_shared = true
		g.ccompiler = 'gcc'
		g.set_target(pref.Target{ os: target_os, pointer_bits: 64 })
		assert g.exported_symbol_attribute() == '__attribute__((visibility("default"))) ',
			'${target_os} keeps the visibility attribute'
	}
	// And a static build marks nothing at all.
	mut g := FlatGen.new()
	g.set_target(pref.Target{ os: 'windows', pointer_bits: 64 })
	assert g.exported_symbol_attribute() == '', 'a static build exports nothing'
}
