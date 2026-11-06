module os

fn test_environment_wide_block_keeps_non_ascii_and_following_entries() {
	mut units := []u16{}
	for r in ('=Z:=Z:\\work\x00Büro=gebäude\x00日本語=').runes() {
		units << u16(r)
	}
	// U+1F600 uses two UTF-16 units, even on hosts whose wchar_t is 32 bits.
	units << [u16(0xd83d), 0xde00]
	for r in ('\x00last=value=tail\x00empty=\x00\x00').runes() {
		units << u16(r)
	}
	// The fixture owns a complete, double-NUL-terminated UTF-16 block.
	variables := environ_from_wide_block(unsafe { &units[0] })
	assert variables.len == 4
	assert variables['Büro'] == 'gebäude'
	assert variables['日本語'] == '😀'
	assert variables['last'] == 'value=tail'
	assert variables['empty'] == ''
}

@[unsafe]
fn test_environment_wide_block_handles_null_and_empty() {
	assert environ_from_wide_block(unsafe { nil }).len == 0
	zero := [u16(0), 0]!
	assert environ_from_wide_block(unsafe { &zero[0] }).len == 0
}
