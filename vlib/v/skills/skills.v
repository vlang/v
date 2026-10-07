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
import rand
import crypto.sha256
import json2 as json

// entry_file is the file every skill directory must contain. It is both the
// entry point an agent reads first and the marker `catalog` looks for, so a
// directory without one is never offered as a skill.
pub const entry_file = 'SKILL.md'

// origin_file records what a bundle looked like when it was installed, so a
// later bundle change can be told apart from a local edit.
//
// It sits in the install directory's own root rather than inside each skill,
// because `list_files` walks a skill directory and would otherwise count it as a
// skill file.
pub const origin_file = 'origin.json'

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

// find returns the bundled skill called `name`, or none for an invalid name.
pub fn find(vroot string, name string) ?Skill {
	return load(bundled_root(vroot), name)
}

// load reads one skill directory from `root`.
fn load(root string, name string) ?Skill {
	validate_name(name) or { return none }
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
// Its name must match the bundle and its destination must be an immediate child
// of the install directory. File paths must stay within that skill and its bundle
// and refer to regular files.
//
// Without `force`, an already installed skill is skipped and reported as such
// rather than overwritten: an agent must not silently discard local edits to a
// checked-in skill. `dry_run` computes the same result without writing.
//
// A symlink sitting where the skill would go is refused. `os.is_dir` and
// `os.rmdir_all` both follow their argument, so `--force` over a link would
// list and delete the *target's* contents rather than the link. Provenance symlinks and
// non-regular files are refused before installed content is replaced.
pub fn install(skill Skill, dir string, opts InstallOptions) !InstallResult {
	validate_name(skill.name)!
	bundle_name := validate_bundle(skill.directory)!
	if bundle_name != skill.name {
		return error('skill name `${skill.name}` does not match the bundle name `${bundle_name}`')
	}
	dest := os.join_path_single(dir, skill.name)
	if os.is_link(dest) {
		return error('refusing to install over a symlink skill directory `${dest}`')
	}
	if os.exists(dest) && !os.is_dir(dest) {
		return error('refusing to install over a non-directory skill path `${dest}`')
	}
	install_dir := os.real_path(os.abs_path(dir))
	resolved_dest := if os.exists(dest) {
		os.real_path(os.abs_path(dest))
	} else {
		os.join_path_single(install_dir, skill.name)
	}
	if os.dir(resolved_dest) != install_dir {
		return error('skill `${skill.name}` is outside its immediate install directory')
	}
	bundle_dir := os.real_path(os.abs_path(skill.directory))
	// Validate every file before replacing an existing installation.
	for relative in skill.files {
		if os.is_abs_path(relative) || relative.contains('\\') || relative.contains(':')
			|| relative.split('/').any(it in ['', '.', '..']) {
			return error('invalid skill file path `${relative}`')
		}
		source := os.real_path(os.join_path(skill.directory, relative))
		if !source.starts_with(bundle_dir + os.path_separator) {
			return error('skill file `${relative}` is outside its bundle or is not a regular file')
		}
		info := os.stat(source)!
		if info.get_filetype() != .regular {
			return error('skill file `${relative}` is not a regular file')
		}
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
	// Validate provenance before replacing any installed content.
	validate_origin_destination(dir)!
	if !opts.dry_run {
		install_into(dest, skill.files, skill.directory)!
	}
	mut result := InstallResult{
		skill:   skill.name
		path:    os.join_path(dest, entry_file)
		dry_run: opts.dry_run
	}
	for relative in skill.files {
		result.written << relative
	}
	if !opts.dry_run {
		// Recorded after the write, so the digest always describes what is on
		// disk. A failed write leaves the previous record, which then reads as
		// `modified` rather than pretending the old content was kept.
		write_origin(dir, skill.name, InstalledOrigin{
			bundle: skill.name
			digest: content_digest(dest, skill.files)!
		})!
	}
	return result
}

// install_into stages all files beside `dest` before replacing the installation.
fn install_into(dest string, files []string, source_dir string) ! {
	temporary := os.join_path_single(os.dir(dest), '.${os.file_name(dest)}-${rand.uuid_v4()}')
	os.mkdir_all(temporary)!
	defer { os.rmdir_all(temporary) or {} }
	for relative in files {
		source := os.join_path(source_dir, relative)
		target := os.join_path(temporary, relative)
		os.mkdir_all(os.dir(target))!
		os.write_file(target, os.read_file(source)!)!
	}
	swap_into(temporary, dest)!
}

// swap_into moves a staged directory into place and restores the previous one on failure.
// Directory replacement requires moving the old directory aside first on Windows.
fn swap_into(source string, dest string) ! {
	mut aside := ''
	if os.exists(dest) {
		aside = os.join_path_single(os.dir(dest), '.${os.file_name(dest)}.old-${rand.uuid_v4()}')
		os.rename(dest, aside)!
	}
	os.rename(source, dest) or {
		if aside != '' {
			os.rename(aside, dest) or {}
		}
		return err
	}
	if aside != '' {
		os.rmdir_all(aside) or {}
	}
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
	// The record has to go with the directory. Left behind, it would describe a
	// digest for files that no longer exist, and a later reinstall of the same
	// name would be compared against it.
	forget_origin(dir, name)
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
// no longer matches the bundled one. This content comparison cannot distinguish
// local edits from an unchanged installation of an older bundle. Use
// refresh_candidates when deciding which installations can be safely updated.
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

// OriginState is what an installed skill looks like relative to its bundle.
//
// The distinction matters because the two wrong answers are not the same event:
// a bundle that moved on is safe to refresh, while a local edit is the user's
// work. Content comparison alone cannot tell them apart, so `origin_file`
// records the digests that were installed and this reports which case applies.
pub enum OriginState {
	// current means the installed files are exactly what was installed and the
	// bundle still matches it.
	current
	// stale means the installed files are still what was installed, but the
	// bundle has since changed. Refreshing cannot lose anything.
	stale
	// modified means the installed files no longer match what was installed, so
	// they were edited here. Refreshing would discard that, and `update` will not
	// do it without `force`.
	modified
	// unknown means there is no usable proof of unchanged content: unreadable files, an installation
	// from before `origin_file` existed, or one written by hand. Treated as
	// modified, because guessing wrong loses the user's files and guessing the
	// other way only asks for `--force`.
	unknown
}

// str renders a state as the word `v skills list` and `v skills update` report.
pub fn (s OriginState) str() string {
	return match s {
		.current { 'current' }
		.stale { 'stale' }
		.modified { 'modified' }
		.unknown { 'unknown' }
	}
}

// InstalledOrigin is one recorded installation: the digest of each file as it
// was installed, and the bundle that provided them.
struct InstalledOrigin {
	bundle string @[json: bundle]
	digest string @[json: digest]
}

// InstalledOriginFile is the shape of `origin_file`.
struct InstalledOriginFile {
	skills map[string]InstalledOrigin
}

// origin_path is where `dir` keeps its provenance record.
fn origin_path(dir string) string {
	return os.join_path(dir, origin_file)
}

// read_origin returns the record for the skill `name` installed in `dir`, or
// none when there is no usable record.
fn read_origin(dir string, name string) ?InstalledOrigin {
	text := os.read_file(origin_path(dir)) or { return none }
	file := json.decode[InstalledOriginFile](text) or { return none }
	return file.skills[name] or { return none }
}

// read_origin_file returns every recorded installation in `dir`, so one install
// does not drop the entry another skill already has.
fn read_origin_file(dir string) map[string]InstalledOrigin {
	mut skills := map[string]InstalledOrigin{}
	if text := os.read_file(origin_path(dir)) {
		if file := json.decode[InstalledOriginFile](text) {
			skills = file.skills
		}
	}
	return skills
}

// validate_origin_destination refuses links (including dangling ones) and special files.
fn validate_origin_destination(dir string) ! {
	path := origin_path(dir)
	if os.is_link(path) {
		return error('refusing a symlink provenance file `${path}`')
	}
	if os.exists(path) {
		info := os.stat(path)!
		if info.get_filetype() != .regular {
			return error('provenance file `${path}` is not a regular file')
		}
	}
}

// publish_origin writes into a fresh directory and replaces the destination by rename.
// Replacing the file rather than truncating it also leaves hard-linked files untouched.
fn publish_origin(dir string, skills map[string]InstalledOrigin) ! {
	validate_origin_destination(dir)!
	os.mkdir_all(dir)!
	temporary := os.join_path(dir, '.origin-${rand.uuid_v4()}')
	os.mkdir(temporary)!
	defer { os.rmdir_all(temporary) or {} }
	source := os.join_path(temporary, origin_file)
	os.write_file(source, json.encode(InstalledOriginFile{ skills: skills }))!
	validate_origin_destination(dir)!
	destination := origin_path(dir)
	$if windows {
		// Windows rename cannot replace an existing file. Unlinking never truncates its target.
		if os.exists(destination) { os.rm(destination)! }
	}
	os.rename(source, destination)!
}

// write_origin records `record` for the skill `name` as installed in `dir`.
fn write_origin(dir string, name string, record InstalledOrigin) ! {
	mut skills := read_origin_file(dir)
	skills[name] = record
	publish_origin(dir, skills)!
}

// forget_origin drops the record for `name`, so a reinstall is treated as new.
//
// Removing a skill has to do this: a record left behind describes a digest for
// files that no longer exist, and a later reinstall of the same name would be
// compared against it and read as modified.
pub fn forget_origin(dir string, name string) {
	validate_origin_destination(dir) or { return }
	if read_origin(dir, name) == none {
		return
	}
	mut skills := read_origin_file(dir)
	skills.delete(name)
	path := origin_path(dir)
	// The record is removed rather than emptied once the last skill is gone, so
	// an install directory that holds no skills holds nothing of ours either.
	//
	// Failing to tidy up is not worth reporting: the removal already succeeded,
	// and what is left only affects a later comparison, which errs towards
	// asking rather than overwriting.
	if skills.len == 0 {
		os.rm(path) or {}
		return
	}
	publish_origin(dir, skills) or {}
}

// content_digest is one value describing the files `files` names in `directory`.
//
// The digest covers the file names as well as their bytes, so a rename changes
// it, and the files are sorted so the order they were listed in does not.
// An unreadable file returns an error; an incomplete digest cannot prove content is unchanged.
pub fn content_digest(directory string, files []string) !string {
	mut sorted := files.clone()
	sorted.sort()
	mut buf := []u8{}
	for relative in sorted {
		buf << relative.bytes()
		buf << u8(0)
		text := os.read_file(os.join_path(directory, relative))!
		buf << text.bytes()
		buf << u8(0)
	}
	return sha256.hexhash(buf.bytestr())
}

// origin_state reports how the skill `name` installed in `dir` relates to the
// bundle of the same name under `vroot`.
//
// A skill that is no longer bundled is `current`: there is nothing to update it
// to, so it is left to whoever installed it.
pub fn origin_state(vroot string, dir string, name string) OriginState {
	dest := os.join_path_single(dir, name)
	skill := find(vroot, name) or { return OriginState.current }
	now := content_digest(dest, list_files(dest)) or { return OriginState.unknown }
	record := read_origin(dir, name) or { return OriginState.unknown }
	if now == record.digest {
		// What is on disk is what was installed. Whether that is still the right
		// content is the bundle's business.
		bundled := content_digest(skill.directory, skill.files) or { return OriginState.unknown }
		return if bundled == record.digest {
			OriginState.current
		} else {
			OriginState.stale
		}
	}
	return OriginState.modified
}

// refresh_candidates returns the skills installed in `dir` that no longer match
// their bundle, split by whether refreshing them is safe.
//
// `refreshable` holds the `stale` ones: their files are still the ones that were
// installed, so copying the newer bundle over them cannot lose anything.
// `held_back` holds the `modified` and `unknown` ones, where refreshing would
// discard a local edit, or where nothing records what was installed. Both are
// the caller's to decide about, which is why they are not returned as one list.
//
// A skill that is `current` appears in neither.
pub fn refresh_candidates(vroot string, dir string) ([]string, []string) {
	mut refreshable := []string{}
	mut held_back := []string{}
	for name in installed(dir) {
		match origin_state(vroot, dir, name) {
			.current {}
			.stale {
				refreshable << name
			}
			else {
				held_back << name
			}
		}
	}
	return refreshable, held_back
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
