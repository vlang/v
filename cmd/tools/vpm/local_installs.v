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
// of that token in the global cache, never in the project. Both have to agree, so
// a path whose install was deleted or moved by hand cannot pass its authority on
// to whatever the project puts there next: the leftover note is for a directory
// that can no longer answer with its token.

const local_install_token_name = 'vpm_local_install'

fn local_install_records_dir() string {
	return os.join_path(os.vmodules_dir(), '.cache', 'local_installs')
}

// The note is named after the path it stands for, and holds that path, so a stale
// or colliding name can never be read as provenance for another directory.
fn local_install_record_path(install_path string) string {
	return os.join_path(local_install_records_dir(), sha256.hexhash(canonical_install_path(install_path)))
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

fn record_local_install(install_path string) {
	token_bytes := rand.bytes(32) or {
		vpm_error('failed to record the local installation at `${fmt_mod_path(install_path)}`.',
			details: err.msg()
		)
		return
	}
	token := token_bytes.hex()
	os.mkdir_all(local_install_records_dir(), mode: 0o700) or {
		vpm_error('failed to record the local installation at `${fmt_mod_path(install_path)}`.',
			details: err.msg()
		)
		return
	}
	os.write_file(local_install_token_path(install_path), token) or {
		vpm_error('failed to record the local installation at `${fmt_mod_path(install_path)}`.',
			details: err.msg()
		)
		return
	}
	os.write_file(local_install_record_path(install_path), '${canonical_install_path(install_path)}\n${token}\n') or {
		vpm_error('failed to record the local installation at `${fmt_mod_path(install_path)}`.',
			details: err.msg()
		)
	}
}

// The token goes with the directory it sat in, so only the note is left to drop.
fn forget_local_install(install_path string) {
	os.rm(local_install_record_path(install_path)) or {}
}

fn is_recorded_local_install(install_path string) bool {
	record := os.read_file(local_install_record_path(install_path)) or { return false }
	lines := record.split_into_lines()
	if lines.len < 2 || lines[0].trim_space() != canonical_install_path(install_path) {
		return false
	}
	token := lines[1].trim_space()
	if token == '' {
		return false
	}
	stored := os.read_file(local_install_token_path(install_path)) or { return false }
	return stored.trim_space() == token
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
