module main

import os
import crypto.sha256
const compilers = ['gcc', 'msvc', 'tcc']
const w4_runner = 'run_multiwindow_win32_w4.ps1'
const w3_runner = 'run_multiwindow_win32_w3_green.ps1'
const makev_workflow = '.github/workflows/makev_ci.yml'

const test_parameters = ['Oracle', 'NativeTest', 'PublicTest', 'NoOptProbe']
const production_parameters = ['ServiceBackend', 'EventDelivery', 'Win32Backend', 'Win32ServiceBackend', 'Win32ServiceNative']

struct Surface {
	paths                []string
	hashes               []string
	cases                [][]string // [kind, path, name, family]
	production_paths     []string
	production_hashes    map[string][]string
	declared_tuple       string
}

fn sha256_file(path string) string {
	data := os.read_file(path) or { return '' }
	return sha256.hexhash(data)
}

fn sha256_bytes(data string) string {
	return sha256.hexhash(data)
}

fn literals(body string) []string {
	// extract '...' literals
	mut res := []string{}
	mut i := 0
	for i < body.len {
		if body[i] == u8(39) { // '
			end := body.index_after("'", i + 1) or { -1 }
			if end >= 0 {
				res << body[i + 1..end]
				i = end + 1
				continue
			}
		}
		i++
	}
	return res
}

fn parse_array(text string, name string) []string {
	prefix := '$' + name + ' = @('
	start := text.index(prefix) or { return []string{} }
	mut pos := start + prefix.len
	// find closing paren at start of line
	mut i := pos
	mut depth := 1
	for i < text.len && depth > 0 {
		if text[i] == u8(40) { depth++ }
		else if text[i] == u8(41) { depth-- }
		if depth == 0 { break }
		i++
	}
	body := text[pos..i]
	return literals(body)
}

fn parse_surface(text string) !Surface {
	// variables = re.findall(r'\$(\w+)', one(r'^\$testPaths = @\((.*?)\)$', text))
	// simplified: find $testPaths = @( ... ) and extract $var names
	testpaths_prefix := '$testPaths = @('
	tp_start := text.index(testpaths_prefix) or { return error('missing testPaths') }
	mut tp_end := tp_start + testpaths_prefix.len
	mut depth := 1
	for tp_end < text.len && depth > 0 {
		if text[tp_end] == u8(40) { depth++ }
		else if text[tp_end] == u8(41) { depth-- }
		if depth == 0 { break }
		tp_end++
	}
	tp_body := text[tp_start + testpaths_prefix.len..tp_end]
	mut variables := []string{}
	mut vp := 0
	for {
		idx := tp_body.index_after('$', vp) or { -1 }
		if idx < 0 { break }
		// read word
		mut end := idx + 1
		for end < tp_body.len && (tp_body[end].is_alnum() || tp_body[end] == u8(95)) { end++ }
		if end > idx + 1 {
			variables << tp_body[idx + 1..end]
		}
		vp = end
	}
	if variables != ['oracle', 'nativeTest', 'publicTest', 'noOptProbe'] {
		return error('W4 test-path order changed')
	}
	// paths = [one(r"^\$name = '([^']+)'$", text) for name in variables]
	mut paths := []string{}
	for name in variables {
		needle := '$' + name + " = '"
		idx := text.index(needle) or { return error('missing path ${name}') }
		start := idx + needle.len
		end2 := text.index_after("'", start) or { return error('unterminated path ${name}') }
		paths << text[start..end2]
	}
	mut path_by_var := map[string]string{}
	for i, v in variables { path_by_var[v] = paths[i] }

	// case_body = one(r'^\$cases = @\((.*?)^\)', text)
	cases_prefix := '$cases = @('
	cs_start := text.index(cases_prefix) or { return error('missing cases') }
	mut cs_end := cs_start + cases_prefix.len
	mut cs_depth := 1
	for cs_end < text.len && cs_depth > 0 {
		if text[cs_end] == u8(40) { cs_depth++ }
		else if text[cs_end] == u8(41) { cs_depth-- }
		if cs_depth == 0 { break }
		cs_end++
	}
	case_body := text[cs_start + cases_prefix.len..cs_end]

	// parse raw cases: [pscustomobject]@{ Kind = '...' File = $var Name = '...' Family = '...' }
	mut raw_cases := [][]string{}
	mut pos := 0
	for {
		idx := case_body.index_after('[pscustomobject]@{', pos) or { -1 }
		if idx < 0 { break }
		end := case_body.index_after('}', idx) or { -1 }
		if end < 0 { break }
		inner := case_body[idx + '[pscustomobject]@{'.len..end]
		// parse Kind, File, Name, Family
		mut kind := ''
		mut file_var := ''
		mut name := ''
		mut family := ''
		// Kind = '...'
		if kidx := inner.index("Kind = '") {
			ks := kidx + "Kind = '".len
			ke := inner.index_after("'", ks) or { ks }
			kind = inner[ks..ke]
		}
		// File = $var
		if fidx := inner.index('File = $') {
			fs := fidx + 'File = $'.len
			mut fe := fs
			for fe < inner.len && (inner[fe].is_alnum() || inner[fe] == u8(95)) { fe++ }
			file_var = inner[fs..fe]
		}
		// Name = 'test_...'
		if nidx := inner.index("Name = '") {
			ns := nidx + "Name = '".len
			ne := inner.index_after("'", ns) or { ns }
			name = inner[ns..ne]
		}
		// Family = '...'
		if famidx := inner.index("Family = '") {
			fams := famidx + "Family = '".len
			fame := inner.index_after("'", fams) or { fams }
			family = inner[fams..fame]
		}
		if name.len > 0 {
			raw_cases << [kind, file_var, name, family]
		}
		pos = end + 1
	}
	if raw_cases.len != 8 {
		return error('W4 must retain eight cases, got ${raw_cases.len}')
	}

	// check native/public coverage
	mut kinds := []string{}
	for rc in raw_cases { kinds << rc[0] }
	// expect 7 native + 1 public
	mut native_count := 0
	mut public_count := 0
	for k in kinds {
		if k == 'native' { native_count++ }
		if k == 'public' { public_count++ }
	}
	if native_count != 7 || public_count != 1 {
		return error('W4 native/public case coverage changed')
	}

	// build cases with resolved paths
	mut cases := [][]string{}
	for rc in raw_cases {
		path := path_by_var[rc[1]] or { '' }
		cases << [rc[0], path, rc[2], rc[3]]
	}

	// production surface
	prod_prefix := '$knownSurfaceFiles = @{'
	prod_start := text.index(prod_prefix) or { return error('missing knownSurfaceFiles') }
	mut prod_end := prod_start + prod_prefix.len
	mut prod_depth := 1
	for prod_end < text.len && prod_depth > 0 {
		if text[prod_end] == u8(123) { prod_depth++ } // {
		else if text[prod_end] == u8(125) { prod_depth-- } // }
		if prod_depth == 0 { break }
		prod_end++
	}
	prod_body := text[prod_start + prod_prefix.len..prod_end]

	// parse Red/Green arrays
	mut production_hashes := map[string][]string{}
	for state in ['Red', 'Green'] {
		state_prefix := '    ' + state + ' = @('
		si := prod_body.index(state_prefix) or { continue }
		mut se := si + state_prefix.len
		mut sd := 1
		for se < prod_body.len && sd > 0 {
			if prod_body[se] == u8(40) { sd++ }
			else if prod_body[se] == u8(41) { sd-- }
			if sd == 0 { break }
			se++
		}
		body := prod_body[si + state_prefix.len..se]
		production_hashes[state] = literals(body)
	}

	// array hashes
	test_hashes := parse_array(text, 'knownTestFileHashes')
	prod_paths := parse_array(text, 'surfacePaths')

	// declared tuple
	mut declared_tuple := ''
	tuple_needle := "\$knownTestTupleSha256 = '"
	ti := text.index(tuple_needle) or { -1 }
	if ti >= 0 {
		ts := ti + tuple_needle.len
		te := text.index_after("'", ts) or { ts }
		declared_tuple = text[ts..te]
	}

	surf := Surface{
		paths: paths
		hashes: test_hashes
		cases: cases
		production_paths: prod_paths
		production_hashes: production_hashes
		declared_tuple: declared_tuple
	}

	h := surf.hashes
	if h.len != 4 {
		return error('W4 must pin four test inputs, got ${h.len}')
	}
	pp := surf.production_paths
	if pp.len != 5 {
		return error('W4 must pin five production inputs, got ${pp.len}')
	}
	for _, vals in production_hashes {
		if vals.len != 5 {
			return error('W4 production hash count changed')
		}
	}
	for d in surf.hashes {
		if d.len != 64 { return error('invalid W4 SHA-256') }
	}
	for _, vals in production_hashes {
		for d in vals {
			if d.len != 64 { return error('invalid W4 SHA-256') }
		}
	}

	// tuple hash
	mut records := ['schema=package2-win32-w4-test-tuple-v1']
	for i, p in surf.paths {
		records << 'file=${p}|sha256=${surf.hashes[i]}'
	}
	for c in surf.cases {
		records << 'case=${c[0]}|${c[2]}|${c[3]}'
	}
	tuple_hash := sha256.hexhash(records.join('\n') + '\n')
	if tuple_hash != surf.declared_tuple {
		return error('W4 ordered test tuple mismatch')
	}

	return surf
}

fn validate_shared_pins(surface Surface, w3_text string) ! {
	// parse W3 surface entries: Path = '...' Hash = '...'
	mut entries := map[string]string{}
	mut pos := 0
	for {
		idx := w3_text.index_after("Path = '", pos) or { -1 }
		if idx < 0 { break }
		ps := idx + "Path = '".len
		pe := w3_text.index_after("'", ps) or { break }
		path := w3_text[ps..pe]
		// find Hash = '...' after
		hneedle := "Hash = '"
		hi := w3_text.index_after(hneedle, pe) or { break }
		hs := hi + hneedle.len
		he := w3_text.index_after("'", hs) or { break }
		hash := w3_text[hs..he]
		entries[path] = hash
		pos = he + 1
	}
	for idx in [1, 2] {
		path := surface.paths[idx]
		expected := surface.hashes[idx]
		actual := entries[path] or { '' }
		if actual != expected {
			return error('W3/W4 pin mismatch for ${path}')
		}
	}
}

fn validate_caller(script string, workflow string, compiler string, surface Surface) ! {
	text := script
	// find step
	step_marker := '      - name: Win32 W4 clipboard contracts\n'
	si := workflow.index(step_marker) or { return error('missing W4 step') }
	// find end of step (next '      - ' or EOF)
	mut ei := workflow.index_after('      - ', si + step_marker.len) or { workflow.len }
	// ensure we find the next step at same indent
	step := workflow[si..ei]

	if !step.contains('        shell: pwsh\n') {
		return error('W4 requires PowerShell')
	}
	if !step.contains("          VFLAGS: ''\n") {
		return error('W4 must isolate VFLAGS')
	}
	if step.count('run_multiwindow_win32_w4.ps1') != 1 {
		return error('missing/duplicate W4 invocation')
	}
	// parse -Compiler X
	mut comp := ''
	if ci := step.index('-Compiler ') {
		cs := ci + '-Compiler '.len
		ce := step.index_after('`', cs) or { step.len }
		comp = step[cs..ce].trim_space()
	}
	if comp != compiler {
		return error('wrong W4 compiler')
	}
	if !step.contains('-Expectation Green') {
		return error('W4 must expect Green')
	}
	// parse -ExpectedXxxSha256 'hash'
	mut supplied := map[string]string{}
	mut pos := 0
	for {
		idx := step.index_after('-Expected', pos) or { -1 }
		if idx < 0 { break }
		// find the param name and hash
		// pattern: -ExpectedNameSha256 'hash'
		mut end := idx
		for end < step.len && step[end] != u8(32) && step[end] != u8(10) && step[end] != u8(96) { end++ }
		param := step[idx + 1..end] // Expected...Sha256
		// find hash
		hi := step.index_after("'", end) or { -1 }
		if hi < 0 { break }
		hs := hi + 1
		he := step.index_after("'", hs) or { -1 }
		if he < 0 { break }
		hash := step[hs..he]
		supplied[param] = hash.to_lower()
		pos = he + 1
	}
	// build expected
	mut expected := map[string]string{}
	for i, name in test_parameters {
		expected['Expected${name}Sha256'] = surface.hashes[i]
	}
	for i, name in production_parameters {
		expected['Expected${name}Sha256'] = surface.production_hashes['Green'][i]
	}
	for state in ['Red', 'Green'] {
		// production hash for state
		mut records := ['schema=package2-win32-w4-production-surface-v1', 'expectation=${state}']
		for i, p in surface.production_paths {
			records << 'file=${p}|sha256=${surface.production_hashes[state][i]}'
		}
		expected['Expected${state}SurfaceSha256'] = sha256.hexhash(records.join('\n') + '\n')
	}
	expected['ExpectedRunnerSha256'] = sha256.hexhash(text)

	if supplied.len != expected.len {
		return error('W4 caller pins differ for ${compiler}: count mismatch')
	}
	for k, v in expected {
		if supplied[k] != v {
			return error('W4 caller pins differ for ${compiler}')
		}
	}
	return
}

fn validate_sources(surface Surface, read_fn fn (string) string) ! {
	mut expected := map[string]string{}
	for i, p in surface.paths {
		expected[p] = surface.hashes[i]
	}
	for i, p in surface.production_paths {
		expected[p] = surface.production_hashes['Green'][i]
	}
	for p, digest in expected {
		actual := sha256.hexhash(read_fn(p))
		if actual != digest {
			return error('W4 source hash mismatch for ${p}: expected=${digest} actual=${actual}')
		}
	}
	for c in surface.cases {
		content := read_fn(c[1])
		if !content.contains('fn ${c[2]}(') {
			return error('W4 test must be discovered exactly once: ${c[2]} in ${c[1]}')
		}
	}
}

fn main() {
	root := os.join_path(@DIR, '..', '..', '..')
	runner_path := os.join_path(root, '.github', 'workflows', w4_runner)
	runner := os.read_file(runner_path) or {
		eprintln('cannot read runner: ${err}')
		exit(1)
	}
	text := runner
	surface := parse_surface(text) or {
		eprintln('parse_surface failed: ${err}')
		exit(1)
	}
	println('W4 surface: paths=${surface.paths.len} cases=${surface.cases.len}')

	// shared pins
	w3_path := os.join_path(root, '.github', 'workflows', w3_runner)
	w3_text := os.read_file(w3_path) or { '' }
	validate_shared_pins(surface, w3_text) or {
		eprintln('shared pins failed: ${err}')
		exit(1)
	}
	println('shared pins: OK')

	// callers
	for cc in compilers {
		wf_path := os.join_path(root, '.github', 'workflows', 'windows_ci_${cc}.yml')
		wf := os.read_file(wf_path) or { continue }
		validate_caller(runner, wf, cc, surface) or {
			eprintln('${cc} caller failed: ${err}')
			exit(1)
		}
	}
	println('callers: OK')

	// sources
	validate_sources(surface, fn [root] (p string) string {
		data := os.read_file(os.join_path(root, p)) or { return '' }
		return data
	}) or {
		eprintln('sources failed: ${err}')
		exit(1)
	}
	println('sources: OK')

	println('W4 integrity validation: PASSED')
}
