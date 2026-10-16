module main

import os

struct WipeCacheOptions {
	dry_run           bool
	verbose           bool
	vcache            bool
	vtmp              bool
	v3cache           bool
	tools             bool
	explicit_selector bool
	show_help         bool
}

const wipe_cache_help_text = 'Remove V cache folders (VCACHE, VTMP, V3CACHE, and compiled tool caches).

Usage:
  v wipe-cache [options]

Options:
  -n, --dry-run   Show what would be wiped without deleting any files.
  -v, --verbose   Show detailed output, skipped paths, and tool cache diagnostics.
      --tools     Wipe only compiled tool caches (VTOOLS_CACHE_DIR, ~/.cache/v/tools,
                  <VTMP>/tools, ~/.v/tools, and moved-aside .untrusted-* folders).
      --vcache    Wipe only the V module/object cache (VCACHE or ~/.vmodules/.cache).
      --vtmp      Wipe only the V temporary folder (VTMP).
      --v3cache   Wipe only the V3 module and object caches (V3CACHE).
  -h, --help      Display this help message.'

fn main() {
	passed := os.args[1..]
	command_args := if passed.len > 0 && passed[0] == 'wipe-cache' {
		passed[1..]
	} else {
		passed
	}
	opts := parse_wipe_cache_options(command_args) or {
		eprintln(err.msg())
		eprintln(wipe_cache_help_text)
		exit(1)
	}
	if opts.show_help {
		println(wipe_cache_help_text)
		return
	}
	for line in wipe_caches(opts) {
		println(line)
	}
}

fn parse_wipe_cache_options(command_args []string) !WipeCacheOptions {
	mut dry_run := false
	mut verbose := false
	mut vcache := false
	mut vtmp := false
	mut v3cache := false
	mut tools := false
	mut explicit_selector := false
	mut show_help := false
	for arg in command_args {
		match arg {
			'-n', '-dry-run', '--dry-run' {
				dry_run = true
			}
			'-v', '-verbose', '--verbose' {
				verbose = true
			}
			'-tools', '--tools' {
				tools = true
				explicit_selector = true
			}
			'-vcache', '--vcache' {
				vcache = true
				explicit_selector = true
			}
			'-vtmp', '--vtmp' {
				vtmp = true
				explicit_selector = true
			}
			'-v3cache', '--v3cache' {
				v3cache = true
				explicit_selector = true
			}
			'-h', '-help', '--help' {
				show_help = true
			}
			else {
				return error('v wipe-cache: unknown option `${arg}`')
			}
		}
	}
	if !explicit_selector {
		vcache = true
		vtmp = true
		v3cache = true
		tools = true
	}
	return WipeCacheOptions{
		dry_run:           dry_run
		verbose:           verbose
		vcache:            vcache
		vtmp:              vtmp
		v3cache:           v3cache
		tools:             tools
		explicit_selector: explicit_selector
		show_help:         show_help
	}
}

fn tool_cache_default_candidates() []string {
	mut candidates := [
		os.join_path(os.cache_dir(), 'v', 'tools'),
		os.join_path(os.vtmp_dir(), 'tools'),
	]
	home := os.home_dir().trim_space()
	if home != '' {
		candidates << os.join_path(home, '.v', 'tools')
	}
	return candidates
}

fn append_unique_wipe_target(path string, mut targets []string, mut seen map[string]bool) {
	trimmed := path.trim_space()
	if trimmed == '' {
		return
	}
	key := os.abs_path(trimmed)
	if seen[key] {
		return
	}
	seen[key] = true
	targets << trimmed
}

fn append_untrusted_tool_cache_siblings(candidate string, mut targets []string, mut seen map[string]bool) {
	parent := os.dir(candidate)
	if !os.is_dir(parent) {
		return
	}
	prefix := '${os.file_name(candidate)}.untrusted-'
	mut entries := os.ls(parent) or { return }
	entries.sort()
	for entry in entries {
		if entry.starts_with(prefix) {
			append_unique_wipe_target(os.join_path(parent, entry), mut targets, mut seen)
		}
	}
}

fn tool_cache_wipe_targets() []string {
	mut targets := []string{}
	mut seen := map[string]bool{}
	if configured := os.getenv_opt('VTOOLS_CACHE_DIR') {
		trimmed := configured.trim_space()
		if trimmed != '' {
			append_unique_wipe_target(trimmed, mut targets, mut seen)
			append_untrusted_tool_cache_siblings(trimmed, mut targets, mut seen)
		}
	}
	for candidate in tool_cache_default_candidates() {
		append_unique_wipe_target(candidate, mut targets, mut seen)
		append_untrusted_tool_cache_siblings(candidate, mut targets, mut seen)
	}
	return targets
}

fn is_v3_cache_subdir(name string) bool {
	if name in ['v3_thirdparty_objs', 'v3_fastc_unit_cache', 'v3_fastc_link_cache', 'v3_crun_cache'] {
		return true
	}
	for prefix in ['v3_module_cache_', 'v3_test_cache_'] {
		if name.starts_with(prefix) {
			suffix := name[prefix.len..]
			if suffix.len > 0 && suffix.bytes().all(it.is_hex_digit()) {
				return true
			}
		}
	}
	return false
}

fn wipe_cache_directory(path string, label string, recreate bool, opts WipeCacheOptions, mut lines []string) {
	if opts.dry_run {
		if os.is_link(path) {
			lines << 'Would wipe ${label} folder ${path} (symbolic link).'
		} else if os.is_dir(path) {
			lines << 'Would wipe ${label} folder ${path}.'
			if opts.verbose {
				mut entries := os.ls(path) or { []string{} }
				entries.sort()
				for entry in entries {
					lines << '  would remove ${os.join_path(path, entry)}'
				}
			}
		} else if os.exists(path) {
			lines << 'Would wipe ${label} folder ${path}.'
		} else {
			lines << 'Would wipe ${label} folder ${path} (does not exist).'
		}
		return
	}
	if os.is_link(path) {
		if opts.verbose {
			lines << '  removing symbolic link ${path}'
		}
		os.rm(path) or {}
	} else if os.is_dir(path) {
		if opts.verbose {
			mut entries := os.ls(path) or { []string{} }
			entries.sort()
			for entry in entries {
				lines << '  removing ${os.join_path(path, entry)}'
			}
		}
		os.rmdir_all(path) or {}
	} else if os.exists(path) {
		os.rm(path) or {}
	} else if opts.verbose {
		lines << '  ${path} does not exist'
	}
	if recreate {
		os.mkdir_all(path) or {}
	}
	lines << '${label} folder ${path} was wiped.'
}

fn wipe_caches(opts WipeCacheOptions) []string {
	mut lines := []string{}
	if opts.vcache {
		cache_path := os.getenv_opt('VCACHE') or { os.join_path(os.vmodules_dir(), '.cache') }
		wipe_cache_directory(cache_path, 'V cache', true, opts, mut lines)
	}
	mut tool_lines := []string{}
	if opts.tools {
		mut wiped_any_tool_cache := false
		for target in tool_cache_wipe_targets() {
			label := if os.file_name(target).contains('.untrusted-') {
				'Moved-aside V tool cache'
			} else {
				'V tool cache'
			}
			if os.exists(target) || os.is_link(target) {
				wiped_any_tool_cache = true
				wipe_cache_directory(target, label, false, opts, mut tool_lines)
			} else if opts.verbose {
				tool_lines << '${label} folder ${target} does not exist, skipping.'
			}
		}
		if opts.explicit_selector && !opts.vcache && !opts.vtmp && !opts.v3cache
			&& !wiped_any_tool_cache && !opts.verbose {
			if opts.dry_run {
				tool_lines << 'No existing V tool cache folders to wipe.'
			} else {
				tool_lines << 'V tool cache folders were already clean.'
			}
		}
	}
	if opts.vtmp {
		wipe_cache_directory(os.vtmp_dir(), 'V tmp.c and tests', true, opts, mut lines)
	}
	if opts.v3cache {
		vtmp_path := os.vtmp_dir()
		configured_v3 := os.getenv('V3CACHE').trim_space()
		if configured_v3 != '' && os.abs_path(configured_v3) != os.abs_path(vtmp_path) {
			wipe_cache_directory(configured_v3, 'V3 cache', true, opts, mut lines)
		} else if !opts.vtmp {
			mut v3_dirs := []string{}
			if os.is_dir(vtmp_path) {
				mut entries := os.ls(vtmp_path) or { []string{} }
				entries.sort()
				for entry in entries {
					if is_v3_cache_subdir(entry) {
						candidate := os.join_path(vtmp_path, entry)
						if os.is_dir(candidate) || os.is_link(candidate) {
							v3_dirs << candidate
						}
					}
				}
			}
			if v3_dirs.len > 0 {
				for dir in v3_dirs {
					wipe_cache_directory(dir, 'V3 cache', false, opts, mut lines)
				}
			} else if opts.dry_run {
				lines << 'Would wipe V3 cache folders in ${vtmp_path} (none exist).'
			} else {
				lines << 'V3 cache folders in ${vtmp_path} were already clean.'
			}
		}
	}
	lines << tool_lines
	return lines
}
