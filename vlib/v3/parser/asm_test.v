module parser

import os
import v3.pref

fn parse_amd64_asm_diagnostics(name string, source string) []Diagnostic {
	path := os.join_path(os.temp_dir(), 'v3_asm_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', 'amd64') or { panic(err) }
	mut p := Parser.new(prefs)
	p.parse_file(path)
	return p.diagnostics
}

fn test_inline_asm_lock_without_instruction_reports_error() {
	diagnostics := parse_amd64_asm_diagnostics('lock_missing_instruction', 'fn main() {
	asm amd64 {
		lock')
	assert diagnostics.len == 1, diagnostics.str()
	assert diagnostics[0].message == 'The lock prefix cannot be used on this instruction'
}

fn test_inline_asm_lock_accepts_supported_suffixed_instruction() {
	diagnostics := parse_amd64_asm_diagnostics('lock_supported_instruction', 'fn main() {
	asm amd64 {
		retry: lock cmpxchgq [rdx], rcx
	}
}
')
	assert diagnostics.len == 0, diagnostics.str()
}

fn test_inline_asm_lock_named_operand_is_not_treated_as_prefix() {
	diagnostics := parse_amd64_asm_diagnostics('lock_named_operand', 'fn main() {
	mut value := 0
	asm amd64 {
		add lock, 1
		; +r (value) as lock
	}
}
')
	assert diagnostics.len == 0, diagnostics.str()
}

fn test_inline_asm_lock_label_is_not_treated_as_prefix() {
	diagnostics := parse_amd64_asm_diagnostics('lock_label', 'fn main() {
	asm amd64 {
		lock:
		jmp lock
	}
}
')
	assert diagnostics.len == 0, diagnostics.str()
}

fn test_inline_asm_lock_after_multiline_comment_reports_error() {
	diagnostics := parse_amd64_asm_diagnostics('lock_after_multiline_comment', 'fn main() {
	asm amd64 {
		nop /*
		comment
		*/ lock mov rax, rbx
	}
}
')
	assert diagnostics.len == 1, diagnostics.str()
	assert diagnostics[0].message == 'The lock prefix cannot be used on this instruction'
}

fn test_inline_asm_lock_before_multiline_comment_reports_error() {
	diagnostics := parse_amd64_asm_diagnostics('lock_before_multiline_comment', 'fn main() {
	asm amd64 {
		lock /*
		comment
		*/ add [rax], 1
	}
}
')
	assert diagnostics.len == 1, diagnostics.str()
	assert diagnostics[0].message == 'The lock prefix cannot be used on this instruction'
}
