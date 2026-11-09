module parser

import os
import v.pref

fn powerpc_asm_diagnostics(source string, arch string) []Diagnostic {
	path := os.join_path(os.temp_dir(), 'v_powerpc_asm_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', arch) or { panic(err) }
	mut p := Parser.new(prefs)
	p.parse_file(path)
	return p.diagnostics
}

fn test_powerpc_raw_assembly_accepts_named_operands_and_top_level_entry() {
	diagnostics := powerpc_asm_diagnostics('module main
asm ppc raw {
    ".globl ppc_entry\\n"
    "ppc_entry: blr\\n"
}
fn counter() u32 {
    mut value := u32(0)
    asm volatile ppc raw {
        "mftb %[value]"
        ; [value] "=r" (value)
        ;
        ; memory
    }
    return value
}
', 'ppc')
	assert diagnostics.len == 0, diagnostics.str()
}

fn test_powerpc_structured_assembly_is_not_silently_lowered_with_another_isa() {
	diagnostics := powerpc_asm_diagnostics('fn main() { asm ppc { sync } }', 'ppc')
	assert diagnostics.any(it.message == 'PowerPC inline assembly requires the `raw` modifier')
}

fn test_powerpc_raw_assembly_still_rejects_wrong_target_and_intel_modifier() {
	wrong_target := powerpc_asm_diagnostics('fn main() { asm ppc raw { "sync" } }', 'arm64')
	assert wrong_target.any(it.message.contains('`asm ppc` block in a build for the `arm64` target'))
	intel := powerpc_asm_diagnostics('fn main() { asm ppc raw intel { "sync" } }', 'ppc')
	assert intel.any(it.message.contains('`intel` assembly modifier is only supported'))
}
