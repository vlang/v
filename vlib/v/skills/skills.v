// Module `skills` owns the bundled agent skills that ship with the V compiler
// and the rules for installing them into a project or a user-wide directory.
//
// A skill is a directory under `vlib/v/skills/<name>/` holding a `SKILL.md`
// with YAML front matter (`name`, `description`) plus optional extra files in
// `references/` and `scripts/`. The bundles are read from the V source tree at
// run time rather than embedded into a binary, so a skill can be reviewed,
// edited and diffed in the repository, and adding one needs no rebuild.
//
// Both `v skills` (`cmd/tools/vskills`) and the `v_skills` MCP tool
// (`cmd/tools/vmcp`) go through this module, so the catalog, the layout and the
// install rules cannot drift apart.
module skills

import os

// entry_file is the file every skill directory must contain. It is both the
// entry point an agent reads first and the marker `catalog` looks for, so a
// directory without one is never offered as a skill.
pub const entry_file = 'SKILL.md'

// max_name_length is the longest a skill name may be, per the Agent Skills spec.
pub const max_name_length = 64

// max_description_length is the longest a skill description may be. The
// description is the only part an agent sees before it decides to load the skill,
// so the spec puts a ceiling on it; a longer one would be truncated anyway.
pub const max_description_length = 1024

// project_dir is the per-project install location, relative to a project root.
// It matches the layout `opencode`, Claude Code and friends already scan.
pub const project_dir = '.agents/skills'

// global_dir is the install location relative to the user's home directory, so
// a skill installed with `--global` applies to every project.
pub const global_dir = '.agents/skills'

// bom is the UTF-8 byte order mark (EF BB BF) some editors write at the start
// of a file. It would otherwise hide the front matter block from `strip_bom`.
const bom = '\xEF\xBB\xBF'

// Skill describes one bundled skill directory.
pub struct Skill {
pub:
	name        string
	description string
	// directory is the path of the bundled skill directory.
	directory string
	// files are the skill's files relative to `directory`, `SKILL.md` first.
	files []string
}

// Scope selects where a skill is installed.
pub enum Scope {
	// project_root installs under `<project root>/.agents/skills`, which is
	// committed with the repository and shared with the whole team.
	project_root
	// home_dir installs under `<home>/.agents/skills`, which applies to every
	// project on the machine for the current user.
	home_dir
}

// InstallOptions is what `install` needs beyond the skill and the target.
pub struct InstallOptions {
pub:
	// force overwrites an already installed skill of the same name.
	force bool
	// dry_run reports what would happen without touching the filesystem.
	dry_run bool
}

// InstallResult reports what one `install` call did.
pub struct InstallResult {
pub:
	skill string
	// path is the installed `SKILL.md`.
	path string
	// skipped is true when the skill was already installed and `force` was off.
	skipped bool
	// dry_run is true when nothing was written.
	dry_run bool
pub mut:
	// written lists the files copied, relative to the skill directory.
	written []string
}

// RemoveResult reports what one `remove` call did.
pub struct RemoveResult {
pub:
	skill string
	// path is the removed directory.
	path    string
	removed bool
}

// bundled_root returns `<vroot>/vlib/v/skills`.
pub fn bundled_root(vroot string) string {
	return os.join_path(vroot, 'vlib', 'v', 'skills')
}

// catalog returns every skill bundled with the compiler, sorted by name.
//
// A directory qualifies when it contains `SKILL.md`; a directory without one is
// skipped rather than reported, so an in-progress bundle cannot break
// `v skills list`.
pub fn catalog(vroot string) []Skill {
	root := bundled_root(vroot)
	if !os.is_dir(root) {
		return []Skill{}
	}
	entries := os.ls(root) or {
		return []Skill{}
	}
	mut names := []string{}
	for entry in entries {
		if entry.starts_with('.') {
			continue
		}
		if !os.is_dir(os.join_path_single(root, entry)) {
			continue
		}
		names << entry
	}
	names.sort()
	mut skills := []Skill{cap: names.len}
	for name in names {
		skill := load(root, name) or { continue }
		skills << skill
	}
	return skills
}

// invalid_bundled returns the bundled skills whose front matter does not satisfy
// the Agent Skills spec, as `"name: reason"` lines.
//
// `catalog` skips what it cannot read, which is right for a listing but would
// leave a malformed bundle invisible. `v skills list` reports these instead, so a
// broken bundle shows up before someone tries to install it.
pub fn invalid_bundled(vroot string) []string {
	root := bundled_root(vroot)
	if !os.is_dir(root) {
		return []
	}
	mut out := []string{}
	entries := os.ls(root) or {
		return out
	}
	names := entries.filter(os.is_dir(os.join_path_single(root, it))).sorted()
	for name in names {
		if name.starts_with('.') {
			continue
		}
		directory := os.join_path_single(root, name)
		if !os.is_file(os.join_path_single(directory, entry_file)) {
			continue
		}
		validate_bundle(directory) or {
			out << '${name}: ${err.msg()}'
			continue
		}
	}
	return out
}

// find returns the bundled skill called `name`.
pub fn find(vroot string, name string) ?Skill {
	return load(bundled_root(vroot), name)
}

// load reads one skill directory from `root`.
fn load(root string, name string) ?Skill {
	directory := os.join_path_single(root, name)
	entry := os.join_path_single(directory, entry_file)
	if !os.is_file(entry) {
		return none
	}
	content := os.read_file(entry) or {
		return none
	}
	front := parse_front_matter(content) or {
		return none
	}
	return Skill{
		name:        name
		description: front['description'] or { '' }
		directory:   directory
		files:       list_files(directory)
	}
}

// validate_bundle checks one skill directory against the Agent Skills spec and
// returns its name, or an error naming the first rule it breaks.
//
// The name is what an agent matches a task against and the directory is what
// `v skills remove` addresses, so the spec requires the two to agree; a bundle
// where they disagree is one an agent will load under a name the installer cannot
// find again.
//
// Installing checks this too, so a bundle that fails is refused rather than
// copied somewhere an agent will read it.
pub fn validate_bundle(directory string) !string {
	entry := os.join_path_single(directory, entry_file)
	content := os.read_file(entry) or {
		return error('no readable `${entry_file}` in `${directory}`')
	}
	front := parse_front_matter(content) or {
		return error('`${entry_file}` has no front matter carrying both a name and a description')
	}
	declared := front['name'] or { '' }
	name := validate_name(declared)!
	dir_name := os.file_name(directory.trim_right('/\\'))
	if declared != dir_name {
		return error('the front matter name `${declared}` does not match the directory name `${dir_name}`')
	}
	description := front['description'] or { '' }
	if description.len > max_description_length {
		return error('the description is ${description.len} characters, over the ${max_description_length} the spec allows')
	}
	return name
}

// validate_name checks one skill name and returns it, or an error naming the rule
// it breaks.
pub fn validate_name(name string) !string {
	if name.len == 0 {
		return error('the name is empty')
	}
	if name.len > max_name_length {
		return error('the name is ${name.len} characters, over the ${max_name_length} the spec allows')
	}
	if name.starts_with('-') || name.ends_with('-') {
		return error('the name `${name}` must not start or end with a hyphen')
	}
	if name.contains('--') {
		return error('the name `${name}` must not contain consecutive hyphens')
	}
	for c in name {
		lower := c >= `a` && c <= `z`
		digit := c >= `0` && c <= `9`
		if !(lower || digit || c == `-`) {
			return error('the name `${name}` may only contain lowercase letters, digits and hyphens')
		}
	}
	return name
}

// parse_front_matter reads the `name:` and `description:` keys from a SKILL.md
// YAML front matter block.
//
// The parser is deliberately narrow: it accepts the leading `---` block, then
// reads the flat `key: value` keys a skill needs. A missing block or a missing
// key yields none rather than a guessed value, so `v skills add` never installs
// a bundle whose metadata it could not read.
pub fn parse_front_matter(content string) ?map[string]string {
	lines := strip_bom(content).split_into_lines()
	mut start := -1
	for i, line in lines {
		if line.trim_space() == '---' {
			start = i
			break
		}
	}
	if start < 0 {
		return none
	}
	mut fields := map[string]string{}
	for line in lines[start + 1..] {
		if line.trim_space() == '---' {
			break
		}
		trimmed := line.trim_space()
		if trimmed == '' || trimmed.starts_with('#') {
			continue
		}
		colon := trimmed.index(':') or { continue }
		key := trimmed[..colon].trim_space()
		value := unquote(trimmed[colon + 1..].trim_space())
		if key == '' || value == '' {
			continue
		}
		fields[key] = value
	}
	if fields['name'] or { '' } == '' || fields['description'] or { '' } == '' {
		return none
	}
	return fields
}

// unquote removes one layer of matching single or double quotes.
fn unquote(value string) string {
	if value.len < 2 {
		return value
	}
	first := value[0]
	if (first == `'` || first == `"`) && value[value.len - 1] == first {
		return value[1..value.len - 1]
	}
	return value
}

// strip_bom removes a leading byte order mark from `content`.
pub fn strip_bom(content string) string {
	return if content.starts_with(bom) { content[bom.len..] } else { content }
}

// list_files returns every file below `directory` as paths relative to it, with
// `SKILL.md` first. The rest are sorted so a reinstall reports a stable order.
pub fn list_files(directory string) []string {
	entries := os.ls(directory) or {
		return []
	}
	mut rest := []string{}
	for entry in entries {
		for relative in collect_files(directory, entry) {
			if relative == entry_file {
				continue
			}
			rest << relative
		}
	}
	rest.sort()
	mut files := [entry_file]
	files << rest
	return files
}

// collect_files returns the paths below `directory` that start with the relative
// `entry`, recursively.
//
// The relative paths are joined with `/` rather than the platform separator: a
// skill's files are named relative to its own directory, the same names a
// repository tracks, so they must not change shape with the operating system.
// Every `os` call accepts `/` on Windows as well, so nothing has to translate them
// back.
fn collect_files(directory string, entry string) []string {
	full := os.join_path_single(directory, entry)
	if !os.is_dir(full) {
		return [entry]
	}
	mut res := []string{}
	children := os.ls(full) or {
		return res
	}
	for child in children {
		for nested in collect_files(full, child) {
			res << entry + '/' + nested
		}
	}
	return res
}

// target_dir returns the directory a skill installs into for `scope`. `base` is
// the project root for `Scope.project_root` and is ignored for `Scope.home_dir`.
pub fn target_dir(scope Scope, base string) string {
	return match scope {
		.project_root { os.join_path(base, project_dir) }
		.home_dir { os.join_path(os.home_dir(), global_dir) }
	}
}

// install copies `skill` into `dir`.
//
// The bundle is validated first, so a skill whose front matter does not satisfy
// the spec is refused here rather than copied somewhere an agent will read it.
//
// Without `force`, an already installed skill is skipped and reported as such
// rather than overwritten: an agent must not silently discard local edits to a
// checked-in skill. `dry_run` computes the same result without writing.
//
// A symlink sitting where the skill would go is refused. `os.is_dir` and
// `os.rmdir_all` both follow their argument, so `--force` over a link would
// list and delete the *target's* contents rather than the link. That is how an
// install turns into an unrelated directory wipe.
pub fn install(skill Skill, dir string, opts InstallOptions) !InstallResult {
	validate_bundle(skill.directory)!
	dest := os.join_path_single(dir, skill.name)
	if os.is_link(dest) {
		return error('refusing to install over a symlink skill directory `${dest}`')
	}
	already_installed := os.is_dir(dest)
	if already_installed && !opts.force {
		return InstallResult{
			skill:   skill.name
			path:    os.join_path(dest, entry_file)
			skipped: true
			dry_run: opts.dry_run
		}
	}
	if already_installed && !opts.dry_run {
		os.rmdir_all(dest)!
	}
	mut result := InstallResult{
		skill:   skill.name
		path:    os.join_path(dest, entry_file)
		dry_run: opts.dry_run
	}
	if !opts.dry_run {
		os.mkdir_all(dest)!
	}
	for relative in skill.files {
		source := os.join_path(skill.directory, relative)
		target := os.join_path(dest, relative)
		if !opts.dry_run {
			os.mkdir_all(os.dir(target))!
			// Every bundled skill file is text, so a read and a write are enough
			// and no platform-specific copy code is needed.
			os.write_file(target, os.read_file(source)!)!
		}
		result.written << relative
	}
	return result
}

// remove deletes an installed skill directory. It reports `removed: false` when
// nothing was installed under that name. Names follow validate_name; symlinks
// and targets outside the immediate install directory are refused.
pub fn remove(dir string, name string) !RemoveResult {
	validate_name(name)!
	dest := os.join_path_single(dir, name)
	if os.is_link(dest) {
		return error('refusing to remove a symlink skill directory `${dest}`')
	}
	if !os.is_dir(dest) {
		return RemoveResult{
			skill: name
			path:  dest
		}
	}
	if os.dir(os.real_path(dest)) != os.real_path(dir) {
		return error('skill `${name}` is outside its immediate install directory')
	}
	os.rmdir_all(dest)!
	return RemoveResult{
		skill:   name
		path:    dest
		removed: true
	}
}

// installed returns the names installed in `dir`, sorted.
pub fn installed(dir string) []string {
	mut names := []string{}
	if !os.is_dir(dir) {
		return names
	}
	entries := os.ls(dir) or {
		return names
	}
	for entry in entries {
		if !os.is_dir(os.join_path_single(dir, entry)) {
			continue
		}
		if !os.is_file(os.join_path(os.join_path_single(dir, entry), entry_file)) {
			continue
		}
		names << entry
	}
	names.sort()
	return names
}

// out_of_date returns the bundled skills installed in `dir` whose installed copy
// no longer matches the bundled one. `v skills list` reports these so an agent
// can offer `v skills add --force` instead of acting on stale guidance.
pub fn out_of_date(vroot string, dir string) []string {
	mut stale := []string{}
	for name in installed(dir) {
		skill := find(vroot, name) or { continue }
		dest := os.join_path_single(dir, name)
		if skill.files.len != list_files(dest).len || differs(skill.directory, dest,
			skill.files)
		{
			stale << name
		}
	}
	return stale
}

// differs reports whether any of `files` has different content in the two
// directories, or is missing from `current`.
fn differs(bundled string, current string, files []string) bool {
	for relative in files {
		want := os.read_file(os.join_path(bundled, relative)) or {
			return true
		}
		got := os.read_file(os.join_path(current, relative)) or {
			return true
		}
		if want != got {
			return true
		}
	}
	return false
}

// relative_to renders `path` relative to `base` with forward slashes, for
// readable tool output. The base itself reads as `.`, and a path outside `base` is
// returned unchanged.
pub fn relative_to(base string, path string) string {
	base_slash := os.to_slash(os.real_path(base))
	slashed := os.to_slash(path)
	if slashed == base_slash {
		return '.'
	}
	if slashed.starts_with(base_slash) {
		return slashed[base_slash.len..].trim_left('/')
	}
	return slashed
}

// human_size renders a byte count compactly, for file listings.
pub fn human_size(bytes int) string {
	if bytes < 1024 {
		return '${bytes} B'
	}
	kb := f64(bytes) / 1024.0
	if kb < 1024.0 {
		return '${kb:.1f} KiB'
	}
	return '${kb / 1024.0:.1f} MiB'
}
