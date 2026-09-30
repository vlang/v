import os

// Each `*_notd_v3_no_parallel.v` file has a `*_d_v3_no_parallel.v` sibling that
// replaces it in `-d v3_no_parallel` builds. A function that only the parallel
// variant defines still compiles in the default build, but breaks the serial one
// as soon as anything outside the variant files calls it.
fn test_no_parallel_variants_define_every_externally_called_function() {
	root := os.dir(@FILE)
	mut checked := 0
	for parallel_path in os.walk_ext(root, '.v') {
		if !parallel_path.ends_with('_notd_v3_no_parallel.v') {
			continue
		}
		serial_path := parallel_path.replace('_notd_v3_no_parallel.v', '_d_v3_no_parallel.v')
		assert os.exists(serial_path), 'missing serial variant for ${parallel_path}'
		serial_fns := declared_fns(os.read_file(serial_path) or { panic(err) })
		module_sources := sibling_module_sources(parallel_path, serial_path)
		for name, is_pub in declared_fns(os.read_file(parallel_path) or { panic(err) }) {
			if name in serial_fns {
				continue
			}
			// Other modules can only reach public functions, so a public one is
			// always required. A private one is required once another file of the
			// same module calls it.
			assert !is_pub, '${os.base(serial_path)} lacks public `${name}` from ${os.base(parallel_path)}'
			for source in module_sources {
				assert !calls_fn(source, name), '${os.base(serial_path)} lacks `${name}`, which ${os.base(parallel_path)} defines and its module calls'
			}
		}
		checked++
	}
	// Guard against the walk silently matching nothing after a rename.
	assert checked >= 5
}

// declared_fns maps the name of every top-level function and method declared in
// `source` to whether it is public, with receivers and generic parameters stripped.
fn declared_fns(source string) map[string]bool {
	mut fns := map[string]bool{}
	for line in source.split_into_lines() {
		is_pub := line.starts_with('pub fn ')
		if !is_pub && !line.starts_with('fn ') {
			continue
		}
		mut rest := line.all_after('fn ')
		if rest.starts_with('(') {
			rest = rest.all_after(') ')
		}
		fns[rest.all_before('(').all_before('[').trim_space()] = is_pub
	}
	return fns
}

// sibling_module_sources returns the serial-build view of every non-test `.v` file
// in the variant pair's directory (its module) other than the two variant files.
fn sibling_module_sources(parallel_path string, serial_path string) []string {
	dir := os.dir(parallel_path)
	mut sources := []string{}
	for entry in os.ls(dir) or { panic(err) } {
		path := os.join_path(dir, entry)
		if !entry.ends_with('.v') || entry.ends_with('_test.v') || path == parallel_path
			|| path == serial_path {
			continue
		}
		sources << strip_parallel_only_blocks(os.read_file(path) or { panic(err) })
	}
	return sources
}

// strip_parallel_only_blocks drops the bodies of `$if !v3_no_parallel ? { ... }`
// blocks, which a serial build never compiles, and keeps any `$else` branch. It
// relies on vfmt indentation: a block ends at the first line that starts with `}`
// at the `$if` line's own indent.
fn strip_parallel_only_blocks(source string) string {
	mut kept := []string{}
	mut skip_indent := -1
	for line in source.split_into_lines() {
		indent := line.len - line.trim_left('\t').len
		trimmed := line.trim_space()
		if skip_indent >= 0 {
			if indent == skip_indent && trimmed.starts_with('}') {
				skip_indent = -1
			}
			continue
		}
		if trimmed.starts_with('\$if !v3_no_parallel') {
			if !trimmed.ends_with('}') {
				skip_indent = indent
			}
			continue
		}
		kept << line
	}
	return kept.join('\n')
}

// calls_fn reports whether `source` contains `name(` as a whole identifier.
fn calls_fn(source string, name string) bool {
	needle := '${name}('
	mut from := 0
	for {
		idx := source.index_after(needle, from) or { return false }
		if idx == 0 || !is_ident_char(source[idx - 1]) {
			return true
		}
		from = idx + 1
	}
	return false
}

fn is_ident_char(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_`
}
