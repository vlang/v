module driver

import os
import v.cmdexec
import v.modulecache

// v3_tcc_compile_input_args returns the arguments of a TinyCC compile-and-link
// command that decide its include search and compilation: everything but the output,
// the inputs and the options of the link.
fn v3_tcc_compile_input_args(tcc_args []string, source_name string) []string {
	mut args := []string{cap: tcc_args.len}
	mut i := 0
	for i < tcc_args.len {
		arg := tcc_args[i]
		clean := arg.trim_space()
		i++
		if clean in ['-o', '-framework', '-rpath', '-L', '-l'] {
			i++
			continue
		}
		if clean == source_name || clean in ['-c', '-shared', '-rdynamic', '-static']
			|| clean.starts_with('-l') || clean.starts_with('-L') || clean.starts_with('-Wl,')
			|| clean.starts_with('-bt') || (!clean.starts_with('-') && v3_path_is_link_input(clean)) {
			continue
		}
		args << arg
	}
	return args
}

// v3_tcc_include_dirs returns the directories that `args` make TinyCC search for
// an included file, each kind in the order it is given: those of `-I`, which it
// searches first, those of `-isystem`, which come after the ones of the
// environment, and its own headers, which come after all of these.
fn v3_tcc_include_dirs(args []string) ([]string, []string, []string) {
	mut first := []string{}
	mut system := []string{}
	mut own := []string{}
	mut i := 0
	for i < args.len {
		clean := args[i].trim_space()
		i++
		if clean == '-I' || clean == '-isystem' {
			if i < args.len {
				dir := args[i].trim_space()
				i++
				if clean == '-I' && dir !in first {
					first << dir
				} else if clean == '-isystem' && dir !in system {
					system << dir
				}
			}
		} else if clean.starts_with('-isystem') {
			dir := clean['-isystem'.len..].trim_space()
			if dir !in system {
				system << dir
			}
		} else if clean.starts_with('-I') {
			dir := clean[2..].trim_space()
			if dir.len > 0 && dir !in first {
				first << dir
			}
		} else if clean.starts_with('-B') {
			dir := os.join_path_single(clean[2..].trim_space(), 'include')
			if dir !in own {
				own << dir
			}
		}
	}
	return first, system, own
}

// v3_tcc_default_include_dirs returns the absolute directories that TinyCC searches
// for an included file without being told to. Those that it gives relative to the
// directory it runs in are below the directory of one build, where nothing is.
// What it answers follows from the compiler, its `-B` and C_INCLUDE_PATH: the
// answer is kept in the module cache for those.
fn v3_tcc_default_include_dirs(manager &modulecache.Manager, tcc_path string, tcc_args []string, cc_dir string) []string {
	mut args := tcc_args.filter(it.trim_space().starts_with('-B'))
	record := os.join_path_single(manager.dir, 'tcc_include_dirs_${c_hash_bytes(u64(1469598103934665603), [
		'v3-tcc-include-dirs-1',
		os.real_path(tcc_path),
		v3_cache_file_identity(tcc_path),
		args.join('\n'),
		os.getenv('C_INCLUDE_PATH'),
	].join('\x00').bytes()).hex()}')
	if manager.enabled {
		if kept := os.read_file(record) {
			if kept.ends_with('complete=1\n') {
				return kept.split_into_lines().filter(it != 'complete=1')
			}
		}
	}
	args << '-print-search-dirs'
	result := cmdexec.run_in(tcc_path, args, cc_dir)
	mut dirs := []string{}
	if result.exit_code != 0 {
		return dirs
	}
	mut in_include := false
	for line in result.output.split_into_lines() {
		if !line.starts_with(' ') {
			in_include = line.trim_space() == 'include:'
			continue
		}
		dir := line.trim_space()
		if in_include && os.is_abs_path(dir) && dir !in dirs {
			dirs << dir
		}
	}
	if manager.enabled && manager.ensure_dir() {
		mut lines := dirs.clone()
		lines << 'complete=1'
		os.write_file(record, lines.join('\n') + '\n') or {}
	}
	return dirs
}

// v3_has_include_names returns the names that `source` asks `__has_include` and
// `__has_include_next` about.
fn v3_has_include_names(source string) []string {
	mut names := []string{}
	mut pos := 0
	for {
		found := source.index_after_('__has_include', pos)
		if found < 0 {
			break
		}
		pos = found + '__has_include'.len
		mut open := pos
		if source[open..].starts_with('_next') {
			open += '_next'.len
		}
		for open < source.len && source[open] in [` `, `\t`] {
			open++
		}
		if open >= source.len || source[open] != `(` {
			continue
		}
		open++
		for open < source.len && source[open] in [` `, `\t`] {
			open++
		}
		if open >= source.len || source[open] !in [`<`, `"`] {
			continue
		}
		close := if source[open] == `<` { `>` } else { `"` }
		mut end := open + 1
		for end < source.len && source[end] != close && source[end] != `\n` {
			end++
		}
		if end < source.len && source[end] == close && end > open + 1 {
			name := source[open + 1..end]
			if name !in names {
				names << name
			}
		}
	}
	return names
}

// v3_environment_dirs returns the directories that the variable `name` of the
// environment lists, as CPATH and C_INCLUDE_PATH do for a C compiler.
fn v3_environment_dirs(name string) []string {
	mut dirs := []string{}
	for dir in os.getenv(name).split(os.path_delimiter) {
		if dir.len > 0 && dir !in dirs {
			dirs << dir
		}
	}
	return dirs
}

// v3_tcc_absolute_include_dirs returns `dirs` as TinyCC reads them when it runs in
// `build_dir`: a relative one is below that directory.
fn v3_tcc_absolute_include_dirs(dirs []string, build_dir string) []string {
	mut absolute := []string{cap: dirs.len}
	for dir in dirs {
		path := if os.is_abs_path(dir) { dir } else { os.norm_path(os.join_path(build_dir, dir)) }
		if path !in absolute {
			absolute << path
		}
	}
	return absolute
}

// v3_tcc_include_search returns where TinyCC looks for an included file when it
// runs with `args` in `cc_dir`, in the order in which it searches: the directories
// of `-I`, those of CPATH, those of `-isystem`, those of C_INCLUDE_PATH, its own
// headers, and the ones of the system, which it tells when it is asked.
fn v3_tcc_include_search(manager &modulecache.Manager, tcc_path string, args []string, cc_dir string) V3IncludeSearch {
	mut dirs, system_dirs, own_dirs := v3_tcc_include_dirs(args)
	dirs << v3_environment_dirs('CPATH')
	dirs << system_dirs
	dirs << v3_environment_dirs('C_INCLUDE_PATH')
	dirs << own_dirs
	dirs << v3_tcc_default_include_dirs(manager, tcc_path, args, cc_dir)
	return V3IncludeSearch{
		dirs: v3_tcc_absolute_include_dirs(dirs, cc_dir)
	}
}
