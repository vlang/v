// The toolchain module locates installed V toolchains and resolves which one a
// `v` invocation should use. It is shared by the launcher and the
// `v toolchain` tool so that both agree on names and config locations.
module toolchain

import os
import v.vmod

pub const dispatch_env = 'V_TOOLCHAIN_DISPATCHED'
pub const debug_env = 'V_TOOLCHAIN_DEBUG'
pub const version_env = 'V_TOOLCHAIN'
pub const root_env = 'V_TOOLCHAINS_DIR'
pub const settings_env = 'V_TOOLCHAIN_SETTINGS'
pub const project_env = 'V_TOOLCHAIN_PROJECT'
pub const upstream_env = 'V_TOOLCHAIN_UPSTREAM'
pub const upstream_url = 'https://github.com/vlang/v'
pub const pin_file = 'v-toolchain.toml'
pub const pin_key = 'toolchain'
pub const settings_key = 'default_toolchain'
pub const default_version = 'stable'

// Selection describes the toolchain a `v` invocation resolved to.
pub struct Selection {
pub:
	requested string // the raw name found in config
	version   string // normalized name used as the install directory
	source    string // where the name came from
}

// configured_root returns where toolchains live. V_TOOLCHAINS_DIR overrides
// the default for tests and custom setups.
pub fn configured_root() string {
	if e := os.getenv_opt(root_env) {
		if e.trim_space() != '' {
			return e
		}
	}
	return os.join_path(os.home_dir(), '.v', 'toolchains')
}

// settings_path returns the file holding the per-user default.
pub fn settings_path() string {
	if e := os.getenv_opt(settings_env) {
		if e.trim_space() != '' {
			return e
		}
	}
	return os.join_path(os.home_dir(), '.v', 'settings.toml')
}

// project_root returns the nearest directory that looks like a repository
// root, or the current directory when none is found. V_TOOLCHAIN_PROJECT
// overrides the search for tests and unusual layouts.
pub fn project_root() string {
	if e := os.getenv_opt(project_env) {
		if e.trim_space() != '' {
			return os.real_path(e)
		}
	}
	mut dir := os.real_path(os.getwd())
	for _ in 0 .. 32 {
		if os.is_dir(os.join_path(dir, '.git')) || os.is_file(os.join_path(dir, '.git'))
			|| os.is_file(os.join_path(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return os.real_path(os.getwd())
}

// pin_path returns the per-project pin file for the current repository.
pub fn pin_path() string {
	return os.join_path(project_root(), pin_file)
}

// configured_upstream returns the git remote to clone from.
pub fn configured_upstream() string {
	if e := os.getenv_opt(upstream_env) {
		if e.trim_space() != '' {
			return e
		}
	}
	return upstream_url
}

// is_version_number reports whether `s` looks like `0.4` or `0.4.11`.
pub fn is_version_number(s string) bool {
	parts := s.split('.')
	if parts.len < 2 || parts.len > 3 {
		return false
	}
	for p in parts {
		if p.len == 0 {
			return false
		}
		for c in p {
			if !c.is_digit() {
				return false
			}
		}
	}
	return true
}

// normalize_version maps user input to an install directory name. Channels
// resolve to master in P0; tags keep their `v` prefix.
pub fn normalize_version(input string) ?string {
	s := input.trim_space().to_lower()
	if s == '' {
		return none
	}
	if s in ['master', 'stable', 'latest', 'nightly'] {
		return 'master'
	}
	mut v := s
	if v.starts_with('v') {
		v = v[1..]
	}
	if !is_version_number(v) {
		return none
	}
	return 'v' + v
}

// toolchain_dir returns the install directory for a normalized version.
pub fn toolchain_dir(root string, version string) string {
	return os.join_path(root, version)
}

// branch_for returns the git branch/tag to clone for a normalized version.
pub fn branch_for(version string) string {
	return version
}

// executable_in returns the built V binary in a checkout, or '' when the
// checkout has not been built yet.
pub fn executable_in(dir string) string {
	name := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	exe := os.join_path(dir, name)
	if os.is_executable(exe) {
		return exe
	}
	return ''
}

// read_key_file reads a top-level quoted string key from a small TOML file.
// Malformed or missing files are treated as "no value" so that a bad config
// cannot break the launcher. This deliberately avoids importing `toml`: the
// launcher already links `v.vmod`, and adding `toml` to `cmd/v` breaks a fresh
// compiler build.
fn read_key_file(path string, key string) ?string {
	text := os.read_file(path) or {
		return none
	}
	mut in_table := false
	for line in text.split('\n') {
		trimmed := line.trim_space()
		if trimmed == '' || trimmed.starts_with('#') {
			continue
		}
		if trimmed.starts_with('[') {
			in_table = true
			continue
		}
		if in_table || !trimmed.starts_with(key) {
			continue
		}
		rest := trimmed[key.len..].trim_space()
		if !rest.starts_with('=') {
			continue
		}
		value := rest[1..].trim_space()
		for quote in ['"', "'"] {
			if value.starts_with(quote) {
				if closing := value[1..].index(quote) {
					inner := value[1..1 + closing]
					if inner != '' {
						return inner
					}
				}
			}
		}
	}
	return none
}

// read_vmod_key reads an unknown string field from a v.mod file using the
// same parser as the compiler. Whole-line matching would miss valid fields
// with a trailing comma or on the same line as `Module {`.
fn read_vmod_key(path string, key string) ?string {
	text := os.read_file(path) or {
		return none
	}
	manifest := vmod.decode(text) or {
		return none
	}
	values := manifest.unknown[key] or {
		return none
	}
	if values.len > 0 && values[0] != '' {
		return values[0]
	}
	return none
}

// read_key reads a string key from a TOML file, returning none when the file
// is missing, malformed, or lacks the key.
pub fn read_key(path string, key string) ?string {
	return read_key_file(path, key)
}

// set_key_in_file sets a top-level string key while preserving the rest of
// the file. It is deliberately small: these config files have one key each.
pub fn set_key_in_file(path string, key string, value string) ! {
	mut lines := []string{}
	if os.is_file(path) {
		lines = (os.read_file(path)!).split('\n')
	}
	entry := '${key} = "${value}"'
	mut replaced := false
	mut in_table := false
	for i, line in lines {
		trimmed := line.trim_space()
		if trimmed.starts_with('[') {
			in_table = true
			continue
		}
		if in_table || !trimmed.starts_with(key) {
			continue
		}
		rest := trimmed[key.len..].trim_space()
		if rest.starts_with('=') {
			lines[i] = entry
			replaced = true
			break
		}
	}
	if !replaced {
		// Keep the key at top level: inserting after a table would put it in
		// that table, where the launcher deliberately does not look for it.
		mut result := []string{}
		mut inserted := false
		for line in lines {
			if !inserted && line.trim_space().starts_with('[') {
				result << entry
				inserted = true
			}
			result << line
		}
		if !inserted {
			result << entry
		}
		lines = result.clone()
	}
	os.mkdir_all(os.dir(path))!
	os.write_file(path, lines.join('\n').trim_right('\n') + '\n')!
}

fn selection_from(raw string, source string) ?Selection {
	name := normalize_version(raw) or {
		return none
	}
	return Selection{
		requested: raw.trim_space()
		version:   name
		source:    source
	}
}

// select resolves the active toolchain. The order is V_TOOLCHAIN, the
// project pin (v-toolchain.toml then v.mod), the per-user default, and
// finally `stable`.
pub fn select() Selection {
	if raw := os.getenv_opt(version_env) {
		if selection := selection_from(raw, version_env) {
			return selection
		}
	}
	pin := pin_path()
	if os.is_file(pin) {
		if raw := read_key_file(pin, pin_key) {
			if selection := selection_from(raw, pin) {
				return selection
			}
		}
	}
	manifest_path := os.join_path(project_root(), 'v.mod')
	if os.is_file(manifest_path) {
		if raw := read_vmod_key(manifest_path, pin_key) {
			if selection := selection_from(raw, manifest_path) {
				return selection
			}
		}
	}
	settings := settings_path()
	if os.is_file(settings) {
		if raw := read_key_file(settings, settings_key) {
			if selection := selection_from(raw, settings) {
				return selection
			}
		}
	}
	return Selection{
		requested: default_version
		version:   normalize_version(default_version) or { 'master' }
		source:    'default'
	}
}

// debug_enabled reports whether the launcher should explain why it did not
// switch toolchains.
pub fn debug_enabled() bool {
	return os.getenv(debug_env) == '1'
}
