module util

fn test_escape_sequence_and_capital_helpers() {
	assert is_escape_sequence(`n`)
	assert is_escape_sequence(`\\`)
	assert !is_escape_sequence(`q`)
	assert contains_capital('lowerCase')
	assert !contains_capital('lower_case')
}

fn test_new_suggestion_bounds_candidate_storage() {
	mut possibilities := []string{cap: int(max_suggestions_limit) + 1}
	for i in 0 .. int(max_suggestions_limit) + 1 {
		possibilities << 'candidate_${i}'
	}
	suggestion := new_suggestion('missing_name', possibilities, SuggestionParams{})
	assert suggestion.known.len == max_suggestions_limit
	assert suggestion.known.cap == max_suggestions_limit
	assert suggestion.known.last().value.len > 0
}

fn test_githash_reads_repository_head() {
	hash := githash(@VMODROOT)!
	assert hash.len == 7
}

fn test_parse_inline_asm_header_reads_arch_and_modifiers() {
	plain := parse_inline_asm_header('asm amd64 ')
	assert plain.arch == 'amd64'
	assert !plain.is_raw
	assert !plain.is_intel
	assert !plain.is_volatile
	assert !plain.is_goto

	modified := parse_inline_asm_header('asm volatile amd64 raw intel ')
	assert modified.arch == 'amd64'
	assert modified.is_volatile
	assert modified.is_raw
	assert modified.is_intel

	goto_header := parse_inline_asm_header('asm goto amd64 ')
	assert goto_header.arch == 'amd64'
	assert goto_header.is_goto
}

fn test_asm_register_names_cover_the_supported_architectures() {
	amd64 := asm_register_names('amd64')
	assert 'rax' in amd64
	assert 'r15d' in amd64
	assert 'zmm31' in amd64
	assert 'k7' in amd64
	assert 'tmm7' in amd64
	assert 'st' in amd64
	assert 'eiz' in amd64
	assert 'riz' in amd64
	assert 'bnd0' in amd64
	assert 'bnd3' in amd64
	assert 'bnd4' !in amd64
	assert 'r7' !in amd64
	assert 'r16' in amd64
	assert 'r31d' in amd64
	assert 'r32' !in amd64
	assert 'mm7' in amd64
	assert 'mm8' !in amd64
	assert 'st7' in amd64
	assert 'st8' !in amd64

	i386 := asm_register_names('i386')
	assert 'eax' in i386
	assert 'mxcsr' in i386

	arm64 := asm_register_names('arm64')
	assert 'x0' in arm64
	assert 'w30' in arm64
	assert 'x31' in arm64
	assert 'w31' in arm64
	assert 'sp' in arm64
	assert 'fpsr' in arm64
	assert 'daif' in arm64
	assert 'za' in arm64
	assert 'za0' in arm64
	assert 'za15' in arm64
	assert 'za0h' in arm64
	assert 'za15v' in arm64
	assert 'zt0' in arm64
	assert 'pn8' in arm64
	assert 'pn15' in arm64
	assert 'rax' in asm_register_names('x86_64')

	assert asm_register_names('ppc64le').len == 0
}

fn test_asm_clobber_special_names() {
	assert asm_clobber_is_special('cc')
	assert asm_clobber_is_special('memory')
	assert asm_clobber_is_special('redzone')
}

fn test_closest_asm_register_only_suggests_near_matches() {
	registers := asm_register_names('amd64')
	assert closest_asm_register('raxx', registers) or { '' } == 'rax'
	assert closest_asm_register('xmm01', registers) or { '' } == 'xmm1'
	assert closest_asm_register('rax', registers) or { '' } != 'rax'
	assert closest_asm_register('some_symbol_name', registers) == none
}
