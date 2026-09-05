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

	modified := parse_inline_asm_header('asm volatile amd64 raw intel ')
	assert modified.arch == 'amd64'
	assert modified.is_volatile
	assert modified.is_raw
	assert modified.is_intel
}

fn test_asm_register_names_cover_the_supported_architectures() {
	amd64 := asm_register_names('amd64')
	assert 'rax' in amd64
	assert 'r15d' in amd64
	assert 'zmm31' in amd64
	assert 'k7' in amd64

	i386 := asm_register_names('i386')
	assert 'eax' in i386
	assert 'mxcsr' in i386

	arm64 := asm_register_names('arm64')
	assert 'x0' in arm64
	assert 'w30' in arm64
	assert 'sp' in arm64

	assert asm_register_names('ppc64le').len == 0
}

fn test_closest_asm_register_only_suggests_near_matches() {
	registers := asm_register_names('amd64')
	assert closest_asm_register('raxx', registers) or { '' } == 'rax'
	assert closest_asm_register('xmm01', registers) or { '' } == 'xmm1'
	assert closest_asm_register('rax', registers) or { '' } != 'rax'
	assert closest_asm_register('some_symbol_name', registers) == none
}
