module main

import crypto.rand
import crypto.sha256
import os

// `v install --local` puts a package in the project's own module lookup root, so
// installed packages sit next to the modules the project writes by hand. Version
// control does not tell those apart -- a project may well keep a module of its
// own as a submodule or a manual clone -- so VPM writes down what it installed
// there, and only a directory that still answers with that record is VPM's to
// update or remove.
//
// The record has two halves: a token inside the checkout VPM made, and the note
// of that token in the user's cache, never in the project. The note is named
// after the token rather than after a path, so a project can be renamed or moved
// and take its installs with it; the path the note holds is where the install was
// last seen, which is what separates a checkout that moved from a copy of it made
// by hand. Neither half is any use alone: a path emptied and filled again by the
// project cannot answer with the token, so it inherits nothing.

const local_install_token_name = 'vpm_local_install'

// The notes live in the user's cache, not under the module directory: a local
// install goes to the project whatever `VMODULES` says, so a record kept beside
// the global module store would be lost the moment that store moved, leaving the
// install unmanageable for want of a note written under another name.
// `VPM_LOCAL_INSTALLS` points them elsewhere, which is what the tests use to keep
// a run of their own.
const local_installs_dir_env = 'VPM_LOCAL_INSTALLS'

fn local_install_records_dir() string {
	if custom := os.getenv_opt(local_installs_dir_env) {
		if custom.trim_space() != '' {
			return custom.trim_space()
		}
	}
	return os.join_path(os.cache_dir(), 'v', 'local_installs')
}

fn local_install_record_path(token string) string {
	return os.join_path(local_install_records_dir(), sha256.hexhash(token))
}

fn canonical_install_path(install_path string) string {
	return real_path_with_missing_suffix(install_path).replace('\\', '/')
}

// The token lives in the VCS directory of the checkout, where it travels with the
// checkout and stays out of `git status` -- an untracked file in the working tree
// would make every locally installed module look like it has work to lose. A
// checkout without a VCS directory of its own keeps the token beside its sources.
fn local_install_token_path(install_path string) string {
	for vcs_dir in ['.git', '.hg'] {
		candidate := os.join_path(install_path, vcs_dir)
		if os.is_dir(candidate) {
			return os.join_path(candidate, local_install_token_name)
		}
	}
	return os.join_path(install_path, '.${local_install_token_name}')
}

fn symlinked_vcs_metadata(install_path string) ?string {
	for vcs_dir in ['.git', '.hg'] {
		if os.is_link(os.join_path_single(install_path, vcs_dir)) {
			return vcs_dir
		}
	}
	return none
}

fn read_local_install_token(install_path string) ?string {
	token := os.read_file(local_install_token_path(install_path)) or { return none }
	trimmed := token.trim_space()
	if trimmed == '' {
		return none
	}
	return trimmed
}

// An install VPM cannot record is one it could never update or remove again, so
// the caller has to treat a failure here as a failed installation.
fn record_local_install(install_path string) ! {
	token := rand.bytes(32)!.hex()
	os.mkdir_all(local_install_records_dir(), mode: 0o700)!
	os.write_file(local_install_token_path(install_path), token)!
	os.write_file(local_install_record_path(token), canonical_install_path(install_path))!
}

fn forget_local_install(token string) {
	os.rm(local_install_record_path(token)) or {}
}

// remove_installed_dir deletes a module directory and the record that it was a
// local install. The token is read first, since it lives inside the directory,
// and the note is only dropped once the directory is really gone: a removal that
// failed has to leave a retry possible. A removal that got partway through may
// have taken the token with it, so what is left keeps a copy of it and stays
// VPM's to finish.
fn remove_installed_dir(module_path string) ! {
	token := read_local_install_token(module_path) or { '' }
	rmdir_all(module_path) or {
		if token != '' && os.is_dir(module_path) && read_local_install_token(module_path) == none {
			os.write_file(local_install_token_path(module_path), token) or {}
		}
		return err
	}
	if token != '' {
		forget_local_install(token)
	}
}

fn is_recorded_local_install(install_path string) bool {
	token := read_local_install_token(install_path) or { return false }
	note := os.read_file(local_install_record_path(token)) or { return false }
	recorded := note.trim_space()
	if recorded == '' {
		return false
	}
	canonical := canonical_install_path(install_path)
	if recorded == canonical {
		return true
	}
	// The note names another path. Either the install moved there from here, with
	// the project around it, or this is a copy of it: the original answering with
	// the same token is what tells the two apart. A copy is not the install, and
	// inherits nothing; a move is the same checkout, so the note follows it.
	if original_token := read_local_install_token(recorded) {
		if original_token == token && !same_filesystem_object(recorded, install_path) {
			return false
		}
	}
	os.write_file(local_install_record_path(token), canonical) or { return false }
	return true
}

// Whether two paths lead to the same thing on disk. A project renamed in case
// alone keeps both spellings working on a case-insensitive filesystem, and the
// old one would otherwise read as a second copy of the install rather than as
// the install itself.
fn same_filesystem_object(left string, right string) bool {
	left_canonical := canonical_install_path(left)
	right_canonical := canonical_install_path(right)
	if left_canonical == right_canonical {
		return true
	}
	left_stat := os.stat(left) or { return false }
	right_stat := os.stat(right) or { return false }
	if left_stat.inode == 0 || right_stat.inode == 0 {
		// Without inodes to compare -- Windows -- there is the filesystem's own
		// idea of sameness, where a path differs from another by being spelled
		// differently, not by being cased differently.
		return left_canonical.to_lower() == right_canonical.to_lower()
	}
	return left_stat.dev == right_stat.dev && left_stat.inode == right_stat.inode
}

// local_installed_modules lists what VPM installed under a local root, by reading
// its own records rather than by walking the root: that root is the project
// itself, and a walk of it would descend through `.git`, build output,
// `node_modules` and every source directory the project has, to find the handful
// of directories the records already name.
fn local_installed_modules(vmodules_path string) []string {
	root := canonical_install_path(vmodules_path)
	notes := os.ls(local_install_records_dir()) or { return [] }
	mut modules := []string{}
	for note in notes {
		recorded := os.read_file(os.join_path(local_install_records_dir(), note)) or { continue }
		install_path := recorded_install_under(note, recorded.trim_space(), root) or { continue }
		modules << import_path_relative_to(install_path, root)
	}
	modules.sort()
	verbose_println_more(@FILE_LINE, @FN, 'found local modules: ${modules}')
	return modules
}

// A note holds where its install was last seen, and is named after the token in
// it, so a directory proves itself by carrying a token that hashes to that name.
fn install_dir_matches_note(install_path string, note string) bool {
	token := read_local_install_token(install_path) or { return false }
	return sha256.hexhash(token) == note
}

// Records are written and compared in one convention: absolute, with `/` for a
// separator, on every host. `os.join_path` and `path_is_below` would hand back
// `\` on Windows, where a record written that way could not be split on `/`
// again, and the same checkout would read as a stranger the next time.
fn canonical_path_is_below(path string, root string) bool {
	if path == root {
		return false
	}
	boundary := if root.ends_with('/') { root } else { root + '/' }
	return path.starts_with(boundary)
}

// Where the note's install is under this root, if it is here at all. A project
// that was renamed or moved took its installs with it, keeping their path inside
// it, so the same tail under this root is where they went -- a couple of reads
// per record, still no walk of the project.
fn recorded_install_under(note string, recorded string, root string) ?string {
	if recorded == '' {
		return none
	}
	if canonical_path_is_below(recorded, root) && install_dir_matches_note(recorded, note) {
		return recorded
	}
	parts := recorded.split('/')
	for i in 1 .. parts.len {
		candidate := canonical_install_path(os.join_path(root, ...parts[i..]))
		if candidate == recorded {
			continue
		}
		if install_dir_matches_note(candidate, note) {
			// A checkout that answers with the token is not necessarily the one the
			// note stands for: copy a project and every install in it answers the
			// same. The original still being there is what settles it, exactly as
			// it does when a command asks about one directory.
			if install_dir_matches_note(recorded, note) && !same_filesystem_object(recorded, candidate) {
				return none
			}
			// Seen at a new path: the note follows the install it stands for.
			os.write_file(os.join_path(local_install_records_dir(), note), candidate) or {
				return none
			}
			return candidate
		}
	}
	return none
}

// vpm_owns_module_dir reports whether a directory is VPM's to act on. The global
// modules directory holds nothing but installed packages, so everything in it
// qualifies, including a checkout whose VCS directory is already gone. A local
// root is shared with the project's own source, so only a recorded install does,
// and only where the project itself is: a name in the root that leads out of it,
// through a symlink, is something else's, whatever it turns out to be.
fn vpm_owns_module_dir(module_path string) bool {
	if !settings.is_local {
		return true
	}
	if !local_root_contains(module_path) {
		return false
	}
	return is_recorded_local_install(module_path)
}

// Whether a path resolves to somewhere inside the local root. Resolving first is
// the point: `<project>/foo` may be a symlink to a checkout elsewhere, and the
// commands that act on it -- remove above all -- act on what it resolves to.
fn local_root_contains(module_path string) bool {
	return canonical_path_is_below(canonical_install_path(module_path), canonical_install_path(settings.vmodules_path))
}

fn not_installed_by_vpm_details() string {
	return "A local install shares the module lookup root with the project's own modules, so only what VPM installed there is VPM's to touch. Nothing records this directory as such an install: it may be a module the project keeps itself, even when it is a submodule or a clone. If an older V installed it, adopt it with `v install --local --adopt <module>`; otherwise handle it by hand, if that is really what you want."
}

// Packages installed by an older V have no record. They went into the project's
// `modules/` directory, which is not a lookup root anymore, and moving one up
// beside the v.mod says nothing about where it came from -- on disk it is exactly
// a checkout the project could have vendored itself. So adoption is something the
// user says, by name: that is the consent nothing else can supply.
fn vpm_adopt(query []string) {
	if !settings.is_local {
		vpm_error('`--adopt` is only meaningful together with `--local`.',
			details: "The global modules directory needs no record: everything in it is VPM's."
		)
		exit(2)
	}
	if query.len == 0 {
		vpm_error('specify at least one module to adopt.',
			details: 'example: `v install --local --adopt mymod`'
		)
		exit(2)
	}
	mut errors := 0
	for m in query {
		rel_path := normalize_mod_path(m.replace('.', os.path_separator))
		module_path := os.join_path(settings.vmodules_path, rel_path)
		if !os.is_dir(module_path) {
			vpm_error('failed to find `${m}` at `${fmt_mod_path(module_path)}`.')
			errors++
			continue
		}
		if os.is_link(module_path) || !local_root_contains(module_path) {
			vpm_error('refusing to adopt `${m}`: `${fmt_mod_path(module_path)}` leads out of the project.',
				details: "It resolves to `${canonical_install_path(module_path)}`. Adopting it would hand VPM a checkout that is not the project's to update or delete -- and `v remove --local` would delete what the link points at, not the link."
			)
			errors++
			continue
		}
		if install_path_has_symlinked_ancestor(module_path, settings.vmodules_path) {
			vpm_error('refusing to adopt `${m}`: `${fmt_mod_path(module_path)}` is reached through a symlink.',
				details: "A module namespace that is a link can point anywhere tomorrow, so what it holds today is not VPM's to record."
			)
			errors++
			continue
		}
		path := os.real_path(module_path)
		if vcs_dir := symlinked_vcs_metadata(path) {
			vpm_error('refusing to adopt `${m}`: `${fmt_mod_path(path)}` has symlinked `${vcs_dir}` metadata.',
				details: 'Recording ownership through the link would modify VCS metadata outside the checkout, and that record would not travel with the adopted sources.'
			)
			errors++
			continue
		}
		if is_recorded_local_install(path) {
			println('Module `${m}` in ${fmt_mod_path(path)} is already recorded as installed by VPM.')
			continue
		}
		if vcs_used_in_dir(path) == none {
			vpm_error('refusing to adopt `${m}`: `${fmt_mod_path(path)}` is not a checkout.',
				details: 'VPM installs a package by cloning it, so what it installed is a `git` or `hg` checkout. This is plain source, which VPM has no business removing or updating.'
			)
			errors++
			continue
		}
		if os.is_file(os.join_path_single(path, '.git')) {
			vpm_error('refusing to adopt `${m}`: `${fmt_mod_path(path)}` is a Git worktree or submodule.',
				details: 'Its Git metadata belongs to another checkout. Recording ownership beside the module sources would make the checkout dirty, while recording it in the shared Git metadata would not travel with this project.'
			)
			errors++
			continue
		}
		record_local_install(path) or {
			vpm_error('failed to adopt `${m}`.', details: err.msg())
			errors++
			continue
		}
		println('Adopted `${m}` in ${fmt_mod_path(path)}.')
	}
	if errors > 0 {
		exit(1)
	}
}
