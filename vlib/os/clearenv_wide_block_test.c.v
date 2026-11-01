module os

fn test_clearenv_windows_block_keeps_hidden_and_empty_entries() {
	mut units := []u16{}
	for r in ('=Z:=Z:\\work\x00Büro=gebäude\x00emoji=').runes() {
		units << u16(r)
	}
	units << [u16(0xd83d), 0xde00]
	for r in ('\x00empty=\x00last=value=tail\x00\x00').runes() {
		units << u16(r)
	}
	environment := clearenv_windows_environment(unsafe { &units[0] })
	assert environment.len == 5
	assert environment['=Z:'] == 'Z:\\work'
	assert environment['Büro'] == 'gebäude'
	assert environment['emoji'] == '😀'
	assert environment['empty'] == ''
	assert environment['last'] == 'value=tail'
}

fn test_clearenv_windows_block_handles_null_and_empty() {
	assert clearenv_windows_environment(unsafe { nil }).len == 0
	zero := [u16(0), 0]!
	assert clearenv_windows_environment(unsafe { &zero[0] }).len == 0
}

fn test_clearenv_windows_crt_names_snapshot() {
	mut entries := []&u16{}
	for entry in ['cached=value', 'Büro=gebäude', 'empty=', '=Z:=Z:\\work'] {
		entries << entry.to_wide()
	}
	defer {
		for entry in entries {
			unsafe { free(voidptr(entry)) }
		}
	}
	entries << unsafe { &u16(nil) }
	names := clearenv_windows_crt_names(unsafe { &&u16(entries.data) })
	assert names == ['cached', 'Büro', 'empty']
	assert clearenv_windows_crt_names(unsafe { nil }).len == 0
	empty := [unsafe { &u16(nil) }]!
	assert clearenv_windows_crt_names(unsafe { &&u16(&empty[0]) }).len == 0
}
