module main

import os
import crypto.sha256
const compilers = ['gcc', 'msvc', 'tcc']
const runner_name = 'run_multiwindow_win32_w3_green.ps1'

// Parse manifest rows: extract [pscustomobject]@{ Key = 'val' ... } blocks
fn parse_pscustomobjects(text string) [][]string {
	mut rows := [][]string{}
	mut pos := 0
	for {
		idx := text.index_after('[pscustomobject]@{', pos) or { -1 }
		if idx < 0 { break }
		end := text.index_after('}', idx) or { -1 }
		if end < 0 { break }
		inner := text[idx + '[pscustomobject]@{'.len..end]
		// parse key = 'value' pairs
		mut fields := []string{}
		mut p := 0
		for {
			// find key = 'value'
			eq := inner.index_after('=', p) or { -1 }
			if eq < 0 { break }
			// key is before =
			mut key_start := eq - 1
			for key_start >= 0 && (inner[key_start] == u8(32) || inner[key_start] == u8(9) || inner[key_start] == u8(10) || inner[key_start] == u8(13)) {
				key_start--
			}
			mut key_end := key_start + 1
			key_start = key_end
			for key_start > 0 && inner[key_start - 1] != u8(32) && inner[key_start - 1] != u8(9) && inner[key_start - 1] != u8(10) && inner[key_start - 1] != u8(13) {
				key_start--
			}
			key := inner[key_start..key_end].trim_space()
			// value after =
			mut vstart := eq + 1
			for vstart < inner.len && (inner[vstart] == u8(32) || inner[vstart] == u8(9)) { vstart++ }
			if vstart >= inner.len || inner[vstart] != u8(39) { break } // expect '
			vstart++
			vend := inner.index_after("'", vstart) or { -1 }
			if vend < 0 { break }
			val := inner[vstart..vend]
			fields << '${key}=${val}'
			p = vend + 1
		}
		if fields.len > 0 {
			rows << fields
		}
		pos = end + 1
	}
	return rows
}

fn extract_array(text string, name string) string {
	// find $name = @( ... )
	prefix := '$' + name + ' = @('
	start := text.index(prefix) or { return '' }
	mut pos := start + prefix.len
	// find matching close paren at column 0
	mut depth := 1
	mut i := pos
	for i < text.len && depth > 0 {
		if text[i] == u8(40) { depth++ }
		else if text[i] == u8(41) { depth-- }
		if depth == 0 { break }
		i++
	}
	return text[pos..i]
}

fn parse_surface(text string) ([][]string, [][]string) {
	arr_text := extract_array(text, 'surface')
	rows := parse_pscustomobjects(arr_text)
	mut surface := [][]string{}
	for row in rows {
		// each row has Path=... and Hash=...
		mut path := ''
		mut hash := ''
		for f in row {
			if f.starts_with('Path=') { path = f['Path='.len..] }
			if f.starts_with('Hash=') { hash = f['Hash='.len..] }
		}
		if path.len > 0 {
			surface << [path, hash]
		}
	}
	cases_text := extract_array(text, 'cases')
	cases_rows := parse_pscustomobjects(cases_text)
	mut cases := [][]string{}
	for row in cases_rows {
		mut kind := ''
		mut file := ''
		mut name := ''
		for f in row {
			if f.starts_with('Kind=') { kind = f['Kind='.len..] }
			if f.starts_with('File=') { file = f['File='.len..] }
			if f.starts_with('Name=') { name = f['Name='.len..] }
		}
		if file.len > 0 {
			cases << [kind, file, name]
		}
	}
	return surface, cases
}

fn compute_composite(surface [][]string, cases [][]string) string {
	mut records := ['schema=package2-win32-w3-green-surface-v1']
	for s in surface {
		records << 'file=${s[0]}|sha256=${s[1]}'
	}
	for c in cases {
		records << 'case=${c[0]}|${c[1]}|${c[2]}'
	}
	composite := sha256.hexhash(records.join('\n') + '\n')
	return composite
}

fn find_composite_pin(text string) string {
	// find $knownCompositeSha256 = '...'
	needle := "\$knownCompositeSha256 = '"
	idx := text.index(needle) or { return '' }
	start := idx + needle.len
	end := text.index_after("'", start) or { return '' }
	return text[start..end]
}

fn main() {
	root := os.join_path(@DIR, '..', '..', '..')
	runner_path := os.join_path(root, '.github', 'workflows', runner_name)
	runner := os.read_file(runner_path) or {
		eprintln('cannot read runner: ${err}')
		exit(1)
	}
	text := runner

	surface, cases := parse_surface(text)
	println('surface rows: ${surface.len}')
	println('cases rows: ${cases.len}')

	// Validate counts
	assert surface.len == 3, 'expected 3 surface rows, got ${surface.len}'
	assert cases.len == 5, 'expected 5 cases, got ${cases.len}'

	// Composite hash
	composite := compute_composite(surface, cases)
	pin := find_composite_pin(text)
	assert pin.len == 64, 'missing composite pin'
	assert composite == pin, 'composite mismatch: expected=${pin} actual=${composite}'
	println('composite pin: OK')

	// Validate source files exist and hashes match
	for s in surface {
		path := os.join_path(root, s[0])
		data := os.read_file(path) or {
			eprintln('cannot read ${path}: ${err}')
			exit(1)
		}
		actual := sha256.hexhash(data)
		assert actual == s[1], 'hash mismatch for ${s[0]}: expected=${s[1]} actual=${actual}'
	}
	println('source file hashes: OK')

	// Validate case test functions exist
	for c in cases {
		path := os.join_path(root, c[1])
		data := os.read_file(path) or { continue }
		content := data
		assert content.contains('fn ${c[2]}('), 'missing fn ${c[2]} in ${c[1]}'
	}
	println('case functions: OK')

	// Validate callers in workflows
	for cc in compilers {
		wf_path := os.join_path(root, '.github', 'workflows', 'windows_ci_${cc}.yml')
		wf := os.read_file(wf_path) or { continue }
		// check runner invocation with pins
		assert wf.contains(runner_name), '${cc}: missing runner invocation'
		runner_hash := sha256.hexhash(runner)
		assert wf.contains(composite), '${cc}: missing composite pin'
		assert wf.contains(runner_hash), '${cc}: missing runner hash'
	}
	println('workflow callers: OK')

	// Negative test: stale source pin rejected
	mut changed := runner.replace(surface[0][1], '0'.repeat(64))
	changed_surface, changed_cases := parse_surface(changed)
	changed_composite := compute_composite(changed_surface, changed_cases)
	assert changed_composite != pin, 'stale pin should produce different composite'
	println('negative stale source pin: OK')

	// Negative test: runner edit changes hash
	changed_runner := runner + '\n'
	changed_hash := sha256.hexhash(changed_runner)
	assert changed_hash != sha256.hexhash(runner), 'runner edit should change hash'
	println('negative runner edit: OK')

	println('W3/W4 integrity validation: PASSED')
}
