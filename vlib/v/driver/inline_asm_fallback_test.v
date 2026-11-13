module driver

import os
import v.parser

// inline_asm_fallback_requested reports whether a parser diagnostic with `message` makes the
// driver request the macOS compatibility fallback, and what it writes to the marker file.
fn inline_asm_fallback_requested(message string) (bool, string) {
	marker := os.join_path(os.vtmp_dir(), 'inline_asm_fallback_${os.getpid()}')
	os.rm(marker) or {}
	defer {
		os.rm(marker) or {}
	}
	saved := os.getenv(macos_v3_no_fallback_env)
	os.unsetenv(macos_v3_no_fallback_env)
	defer {
		if saved.len > 0 {
			os.setenv(macos_v3_no_fallback_env, saved, true)
		}
	}
	diagnostics := [parser.Diagnostic{
		message: message
	}]
	requested := request_macos_v3_compatibility_fallback(diagnostics, marker)
	payload := os.read_file(marker) or { '' }
	return requested, payload
}

fn test_backend_messages_request_the_inline_asm_fallback() {
	for message in ['inline assembly is not supported by the `wasm` backend',
		'inline assembly is not supported by the C backend for the `riscv64` target; guard the block with `\$if amd64 {}`'] {
		requested, payload := inline_asm_fallback_requested(message)
		assert requested, message
		assert payload == macos_v3_inline_asm_fallback, message
	}
}

fn test_target_mismatch_messages_do_not_request_the_fallback() {
	// No compiler can lower a block for another architecture, so there is nothing to retry.
	for message in [
		'`asm amd64` block in a build for the `arm64` target; guard the block with `\$if amd64 {}` or build with `-arch amd64`',
		'inline assembly must name its architecture, for example `asm arm64 { ... }`',
		'unknown inline assembly architecture `foo`',
		'some other parser error',
	] {
		requested, payload := inline_asm_fallback_requested(message)
		assert !requested, message
		assert payload == '', message
	}
}

fn test_fallback_is_not_requested_without_a_marker_file() {
	requested := request_macos_v3_compatibility_fallback([
		parser.Diagnostic{
			message: 'inline assembly is not supported by the `wasm` backend'
		},
	], '')
	assert !requested
}
