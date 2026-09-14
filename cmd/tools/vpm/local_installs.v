module main

import crypto.sha256
import os

// `v install --local` puts a package in the project's own module lookup root, so
// installed packages sit next to the modules the project writes by hand. VCS
// metadata cannot tell those apart -- a project may well keep a hand-written
// module as a submodule or a manual clone -- so VPM writes down what it installed
// there, and a destructive command only touches a directory it finds in that
// record. The record lives in the global cache, never in the project, and the
// worst a missing one can do is refuse a removal that has to be done by hand.

fn local_install_records_dir() string {
	return os.join_path(os.vmodules_dir(), '.cache', 'local_installs')
}

// The record is named after the path it stands for, and holds that path, so a
// stale or colliding name can never be read as provenance for another directory.
fn local_install_record_path(install_path string) string {
	return os.join_path(local_install_records_dir(), sha256.hexhash(canonical_install_path(install_path)))
}

fn canonical_install_path(install_path string) string {
	return real_path_with_missing_suffix(install_path).replace('\\', '/')
}

fn record_local_install(install_path string) {
	os.mkdir_all(local_install_records_dir(), mode: 0o700) or {
		vpm_error('failed to record the local installation at `${fmt_mod_path(install_path)}`.',
			details: err.msg()
		)
		return
	}
	os.write_file(local_install_record_path(install_path), canonical_install_path(install_path)) or {
		vpm_error('failed to record the local installation at `${fmt_mod_path(install_path)}`.',
			details: err.msg()
		)
	}
}

fn forget_local_install(install_path string) {
	os.rm(local_install_record_path(install_path)) or {}
}

fn is_recorded_local_install(install_path string) bool {
	recorded := os.read_file(local_install_record_path(install_path)) or { return false }
	return recorded.trim_space() == canonical_install_path(install_path)
}

// is_removable_module_dir reports whether VPM may delete a directory. The global
// modules directory holds nothing but installed packages, so everything in it
// stays removable, including a checkout whose VCS directory is already gone. A
// local root is shared with the project's own source, so only what VPM recorded
// installing there may go.
fn is_removable_module_dir(module_path string) bool {
	if !settings.is_local {
		return true
	}
	return is_recorded_local_install(module_path)
}

fn not_installed_by_vpm_details() string {
	return "A local install shares the module lookup root with the project's own modules, so only what VPM installed there is VPM's to delete. Nothing records this directory as such an install: it may be a module the project keeps itself, even when it is a submodule or a clone. Delete it by hand, if that is really what you want."
}
