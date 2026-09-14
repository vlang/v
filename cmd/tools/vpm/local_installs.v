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
// of that token in the global cache, never in the project. The note is named
// after the token rather than after a path, so a project can be renamed or moved
// and take its installs with it; the path the note holds is where the install was
// last seen, which is what separates a checkout that moved from a copy of it made
// by hand. Neither half is any use alone: a path emptied and filled again by the
// project cannot answer with the token, so it inherits nothing.

const local_install_token_name = 'vpm_local_install'

fn local_install_records_dir() string {
	return os.join_path(os.vmodules_dir(), '.cache', 'local_installs')
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
// failed has to leave a retry possible.
fn remove_installed_dir(module_path string) ! {
	token := read_local_install_token(module_path) or { '' }
	rmdir_all(module_path)!
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
		if original_token == token {
			return false
		}
	}
	os.write_file(local_install_record_path(token), canonical) or { return false }
	return true
}

// vpm_owns_module_dir reports whether a directory is VPM's to act on. The global
// modules directory holds nothing but installed packages, so everything in it
// qualifies, including a checkout whose VCS directory is already gone. A local
// root is shared with the project's own source, so only a recorded install does.
fn vpm_owns_module_dir(module_path string) bool {
	if !settings.is_local {
		return true
	}
	return is_recorded_local_install(module_path)
}

fn not_installed_by_vpm_details() string {
	return "A local install shares the module lookup root with the project's own modules, so only what VPM installed there is VPM's to touch. Nothing records this directory as such an install: it may be a module the project keeps itself, even when it is a submodule or a clone. Handle it by hand, if that is really what you want."
}
