module main

import os

fn main() {
	showcc := os.getenv('SHOWCC_OUTPUT')
	expected_tcc := os.getenv('EXPECTED_TCC')
	expected_libgc := os.getenv('EXPECTED_LIBGC')
	expected_rpath := os.getenv('EXPECTED_RPATH')
	expected_binary := os.getenv('EXPECTED_BINARY')
	_ = expected_binary

	prefix := '> C compiler cmd: '
	if showcc.contains('\n') || showcc.contains('\r') {
		eprintln('line breaks are forbidden in -showcc output')
		exit(1)
	}
	if !showcc.starts_with(prefix) {
		eprintln('expected exactly one standalone -showcc compiler command')
		exit(1)
	}
	// count prefix occurrences
	mut count := 0
	mut pos := 0
	for {
		idx := showcc.index_after(prefix, pos) or { -1 }
		if idx < 0 { break }
		count++
		pos = idx + prefix.len
	}
	if count != 1 {
		eprintln('expected exactly one standalone -showcc compiler command')
		exit(1)
	}

	raw := showcc[prefix.len..]
	// raw character allowlist
	for i, ch in raw {
		ok := (ch >= u8(97) && ch <= u8(122)) || (ch >= u8(65) && ch <= u8(90)) ||
			(ch >= u8(48) && ch <= u8(57)) || ' \t/._,+-=:\'"'.contains(ch.ascii_str())
		if !ok {
			eprintln('forbidden raw character U+${ch:04X} at offset ${i}')
			exit(1)
		}
	}

	// basic tokenization (respect double quotes)
	mut tokens := []string{}
	mut cur := ''
	mut in_quote := false
	for i := 0; i < raw.len; i++ {
		ch := raw[i]
		if ch == u8(34) { // "
			in_quote = !in_quote
			cur += ch.ascii_str()
		} else if (ch == u8(32) || ch == u8(9)) && !in_quote {
			if cur.len > 0 {
				tokens << cur
				cur = ''
			}
		} else {
			cur += ch.ascii_str()
		}
	}
	if cur.len > 0 { tokens << cur }
	// check no response-file tokens
	for t in tokens {
		if t.starts_with('@') {
			eprintln('response-file token forbidden: ${t}')
			exit(1)
		}
	}

	// validate expected tcc path appears
	if !raw.contains(expected_tcc) {
		eprintln('expected tcc path not found in command')
		exit(1)
	}
	if !raw.contains(expected_libgc) {
		eprintln('expected libgc path not found')
		exit(1)
	}
	rpath := '-Wl,-rpath,${expected_rpath}'
	if !raw.contains(rpath) {
		eprintln('expected rpath not found')
		exit(1)
	}
	if !raw.contains(expected_binary) {
		eprintln('expected binary not found')
		exit(1)
	}
	println('validation OK')
}
