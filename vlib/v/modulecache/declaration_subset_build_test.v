module modulecache

import os

// Two cold builds of one program, one with each module object given the whole declaration
// header (V3_CACHE_FULL_HEADER=1) and one given what its code names, then the C each module
// object was compiled from is compared: what the subset leaves out must be declarations
// whose names the module object no longer mentions anywhere, and the program must behave as
// before. The cache lives in a directory of its own for each build, so nothing is shared.

const program_source = 'import os\n\nfn main() {\n\tprintln(os.file_name("/a/b.txt"))\n\tprintln(os.join_path("x", "y"))\n}\n'

fn cold_build(root string, mode string, full_header bool) !(string, string) {
	dir := os.join_path(root, mode)
	os.mkdir_all(dir)!
	source := os.join_path(dir, 'main.v')
	os.write_file(source, program_source)!
	cache := os.join_path(dir, 'cache')
	os.mkdir_all(cache)!
	os.setenv('V3CACHE', cache, true)
	os.setenv('TMPDIR', os.join_path(dir, 'tmp'), true)
	os.mkdir_all(os.join_path(dir, 'tmp'))!
	if full_header {
		os.setenv('V3_CACHE_FULL_HEADER', '1', true)
	}
	exe := os.join_path(dir, 'main')
	result := os.exec([@VEXE, '-new-compiler', '-o', exe, source])
	assert result.exit_code == 0, result.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	return cache, run.output
}

// module_sources maps a module name to the text of the C its object was compiled from.
fn module_sources(cache string) map[string]string {
	mut out := map[string]string{}
	for path in os.walk_ext(cache, '.c') {
		if path.contains('.tmp.') || path.contains('.shared_objects') {
			continue
		}
		base := os.file_name(path)
		// <module>_<16 hex>_<16 hex>.c
		stem := base.all_before_last('.c')
		parts := stem.split('_')
		// The two hashes are u64.hex(), which is not zero padded: a hash whose top nibble is
		// zero is shorter than 16 characters, so the test must not insist on 16 (it did, and
		// then counted 3 modules of the 5 an `import os` build leaves).
		if parts.len < 3 || !hex_word(parts[parts.len - 1]) || !hex_word(parts[parts.len - 2]) {
			continue
		}
		name := parts[..parts.len - 2].join('_')
		out[name] = os.read_file(path) or { continue }
	}
	return out
}

// hex_word says whether s is a u64.hex() value: one to sixteen hexadecimal digits.
fn hex_word(s string) bool {
	return s.len in 1 .. 17 && s.bytes().all(it.is_hex_digit())
}

fn identifiers_of(text string) map[string]bool {
	mut ids := map[string]bool{}
	collect_identifiers(text, mut ids)
	return ids
}

fn test_a_subset_build_leaves_out_only_what_nothing_in_the_object_names() {
	root := os.join_path(os.vtmp_dir(), 'declaration_subset_build_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	old_cache := os.getenv('V3CACHE')
	old_tmp := os.getenv('TMPDIR')
	defer {
		os.setenv('V3CACHE', old_cache, true)
		os.setenv('TMPDIR', old_tmp, true)
	}
	inherited_full_header := os.getenv('V3_CACHE_FULL_HEADER')
	full_cache, full_output := cold_build(root, 'full', true)!
	// The subset build runs with the environment the test was started in: started with
	// V3_CACHE_FULL_HEADER=1 it is a full build too, and the size check below fails. That
	// is the control of this test.
	if inherited_full_header == '' {
		os.unsetenv('V3_CACHE_FULL_HEADER')
	} else {
		os.setenv('V3_CACHE_FULL_HEADER', inherited_full_header, true)
	}
	subset_cache, subset_output := cold_build(root, 'subset', false)!
	assert subset_output == full_output
	assert full_output.trim_space() == 'b.txt\nx${os.path_separator}y'
	full := module_sources(full_cache)
	subset := module_sources(subset_cache)
	if full.len == 0 {
		// A host where the module cache builds no module objects (Windows under the bundled
		// tcc, as of this writing) has nothing to compare: say so instead of failing on it.
		eprintln('skipping: the cold build left no module object sources in ${full_cache}; the module cache did not run on this host')
		return
	}
	assert full.len > 3
	assert subset.len == full.len
	mut smaller := 0
	mut dropped_total := 0
	for name, full_text in full {
		sub_text := subset[name] or {
			assert false, 'module ${name} missing from the subset build'
			''
		}
		sub_ids := identifiers_of(sub_text)
		full_lines := full_text.split_into_lines()
		sub_lines := sub_text.split_into_lines()
		// the subset is a subsequence of the full text: nothing is added or moved
		mut j := 0
		mut dropped := []string{}
		for line in full_lines {
			if j < sub_lines.len && sub_lines[j] == line {
				j++
			} else {
				dropped << line
			}
		}
		assert j == sub_lines.len, 'module ${name}: the subset is not a subsequence of the full text'
		for line in dropped {
			declared := droppable_declaration_name(line)
			if declared != '' {
				assert declared !in sub_ids, 'module ${name}: ${declared} is declared in a dropped line and still named in the module object'
			} else if line.trim_space().starts_with('"') {
				// the continuation of a dropped literal definition
			} else {
				symbols := generated_string_symbols(line)
				assert symbols.len > 0 && line.trim_space().starts_with('static'), 'module ${name}: a dropped line that is no declaration and no literal: ${line}'
				for symbol in symbols {
					assert symbol !in sub_ids, 'module ${name}: ${symbol} is defined in a dropped line and still named in the module object'
				}
			}
		}
		dropped_total += dropped.len
		if sub_text.len < full_text.len {
			smaller++
		}
	}
	assert dropped_total > 0, 'the subset build dropped nothing'
	assert smaller * 2 > full.len, 'only ${smaller} of ${full.len} module sources are smaller'
}
