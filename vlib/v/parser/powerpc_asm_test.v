module parser

import os
import v.pref

fn powerpc_asm_diagnostics(source string, arch string, backend string) []Diagnostic {
	path := os.join_path(os.temp_dir(), 'v_powerpc_asm_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', arch) or { panic(err) }
	prefs.backend = backend
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
', 'ppc', 'c')
	assert diagnostics.len == 0, diagnostics.str()
}

fn test_powerpc_structured_assembly_is_not_silently_lowered_with_another_isa() {
	for arch in ['ppc', 'amd64'] {
		for body in ['sync', 'sync ; ; ; memory', '; ; ; r0'] {
			diagnostics := powerpc_asm_diagnostics('fn main() { asm ppc { ${body} } }', arch, 'c')
			assert diagnostics.any(it.message == 'PowerPC inline assembly requires the `raw` modifier'), '${arch}: ${body}: ${diagnostics}'
		}
	}
}

fn test_powerpc_empty_assembly_is_architecture_independent() {
	for arch in ['ppc', 'amd64'] {
		diagnostics := powerpc_asm_diagnostics('fn main() { asm ppc {} }', arch, 'c')
		assert diagnostics.len == 0, '${arch}: ${diagnostics}'
	}
}

fn test_powerpc_memory_only_barrier_is_architecture_independent() {
	for arch in ['ppc', 'amd64'] {
		diagnostics := powerpc_asm_diagnostics('fn main() { asm ppc { ; ; ; memory } }', arch, 'c')
		assert diagnostics.len == 0, '${arch}: ${diagnostics}'
	}
}

fn test_powerpc_empty_assembly_requires_c_backend() {
	for backend in ['amd64', 'arm64', 'wasm', 'js', 'eval', 'fastc'] {
		arch := if backend == 'arm64' { 'arm64' } else { 'amd64' }
		for body in ['', '; ; ; memory'] {
			diagnostics := powerpc_asm_diagnostics('fn main() { asm ppc { ${body} } }', arch,
				backend)
			assert diagnostics.len == 1, '${backend}: ${body}: ${diagnostics}'
			assert diagnostics[0].message == 'PowerPC inline assembly requires the `raw` modifier', '${backend}: ${body}: ${diagnostics}'
		}
	}
}

fn test_powerpc_raw_assembly_still_rejects_wrong_target_and_intel_modifier() {
	wrong_target := powerpc_asm_diagnostics('fn main() { asm ppc raw { "sync" } }', 'arm64', 'c')
	assert wrong_target.any(it.message.contains('`asm ppc` block in a build for the `arm64` target'))
	intel := powerpc_asm_diagnostics('fn main() { asm ppc raw intel { "sync" } }', 'ppc', 'c')
	assert intel.any(it.message.contains('`intel` assembly modifier is only supported'))
}
