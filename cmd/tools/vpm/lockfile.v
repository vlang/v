// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import json2
import os
import time

// The name of the lockfile, recorded next to the `v.mod` of a project.
const lockfile_name = 'v.mod.lock'

// The version of the lockfile format that this vpm writes and reads.
const lockfile_version = 1

// LockedModule records how one dependency of a project was resolved, so that
// the very same sources can be installed again later.
pub struct LockedModule {
pub:
	// requested is the dependency string exactly as it is written in `v.mod`,
	// including any `@version` suffix.
	requested string
	// resolved is the tag that was requested with a `@tag` suffix, or a
	// pseudo-version of the checkout HEAD for dependencies requested without one.
	resolved string
	// revision is the full SHA of the resolved commit.
	revision string
	// url is the source the module was cloned from.
	url string
}

// LockFile holds the resolved revisions of the dependencies of a project.
// The entries are keyed by the module name as written in the `v.mod`
// dependencies, without any `@version` suffix.
pub struct LockFile {
pub:
	version int
mut:
	modules map[string]LockedModule
}

// LockScope collects the resolved dependency revisions of one install run that
// is anchored to a project (a directory with a `v.mod`), so that they can be
// recorded in the lockfile of the project once all installs succeeded. A run
// without a project in scope keeps `active` false, and nothing is locked.
struct LockScope {
mut:
	dir     string
	active  bool
	entries map[string]LockedModule
}

// lockfile_path returns the path of the lockfile of the project in `dir`.
fn lockfile_path(dir string) string {
	return os.join_path(dir, lockfile_name)
}

// lockfile_module_key strips the `@version` suffix from the dependency string
// `dep`, the same way vpm splits a requested version away while installing.
// The result is the name a module is keyed under in a lockfile.
fn lockfile_module_key(dep string) string {
	if dep.starts_with('git@') {
		if dep.count('@') > 1 {
			return dep.all_before_last('@')
		}
		return dep
	}
	ident, _ := dep.rsplit_once('@') or { dep, '' }
	return ident
}

// read_lockfile reads and parses the lockfile of the project in `dir`. It
// returns an error when the file is missing, or does not hold a valid lockfile,
// including one written in a newer format version.
pub fn read_lockfile(dir string) !LockFile {
	path := lockfile_path(dir)
	data := os.read_file(path) or {
		return error('failed to read `${path}`: ${err.msg()}')
	}
	lf := json2.decode[LockFile](data) or {
		return error('failed to parse `${path}`: ${err.msg()}')
	}
	if lf.version > lockfile_version {
		return error('unsupported `${path}` version ${lf.version}; this vpm understands versions up to ${lockfile_version}.')
	}
	return lf
}

// write_lockfile writes `lf` as the lockfile of the project in `dir`.
pub fn write_lockfile(dir string, lf LockFile) ! {
	path := lockfile_path(dir)
	os.write_file(path, json2.encode(lf, prettify: true) + '\n')!
}

// upsert adds the lock entry `m` for the module `name`, replacing any entry
// that was recorded for it before.
pub fn (mut lf LockFile) upsert(m LockedModule, name string) {
	lf.modules[name] = m
}

// remove drops the lock entry of the module `name`. Removing an unknown module
// leaves the lockfile unchanged.
pub fn (mut lf LockFile) remove(name string) {
	lf.modules.delete(name)
}

// pseudo_version returns a deterministic version-like identifier of a commit,
// in the form `v0.0.0-<UTC time of ts>-<first 12 chars of sha>`, e.g.
// `v0.0.0-20240102150405-0123456789ab`. It is recorded as the `resolved`
// version of dependencies that were requested without a tag.
fn pseudo_version(ts i64, sha string) string {
	utc := time.unix(ts).custom_format('YYYYMMDDHHmmss')
	short_sha := if sha.len >= 12 { sha[0..12] } else { sha }
	return 'v0.0.0-${utc}-${short_sha}'
}

// project_lockfile_dir returns the directory of the project in scope for the
// current command, or '' when there is none: the local project root under
// `--local`, or the working directory when it holds a `v.mod`. Only runs that
// resolve the dependencies of a project in scope like this record a lockfile.
fn project_lockfile_dir() string {
	if settings.is_local {
		root := settings.vmodules_path
		if os.exists(os.join_path(root, 'v.mod')) {
			return root
		}
		return ''
	}
	wd := os.real_path(os.getwd())
	if os.exists(os.join_path(wd, 'v.mod')) {
		return wd
	}
	return ''
}

// begin anchors the install run to the project in scope, when there is one,
// and loads the entries of its existing lockfile, so that dependency
// resolution can use the recorded revisions. With `--locked`, a run anchored
// to a project without a lockfile is reported as an error instead.
fn (mut scope LockScope) begin() {
	dir := project_lockfile_dir()
	if dir == '' {
		verbose_println('No project v.mod in scope; not recording a lockfile.')
		return
	}
	scope.dir = dir
	scope.active = true
	scope.entries = map[string]LockedModule{}
	lock_path := lockfile_path(dir)
	if !os.exists(lock_path) {
		if settings.is_locked {
			vpm_error('`--locked` requires a lockfile, but `${fmt_mod_path(lock_path)}` does not exist.',
				details: 'Run `v install` without `--locked` once, to record one first.'
			)
			exit(1)
		}
		return
	}
	lf := read_lockfile(dir) or {
		vpm_error('failed to read `${fmt_mod_path(lock_path)}`.', details: err.msg())
		exit(1)
	}
	scope.entries = lf.modules
}

// record adds the resolved state of a freshly installed or updated module `m`
// to the lock scope of the run. Modules without a dependency string, and
// modules whose revision cannot be determined, like `hg` checkouts, are not
// recorded.
fn (mut scope LockScope) record(m Module) {
	if !scope.active || m.requested == '' {
		return
	}
	revision := head_revision(m.install_path)
	if revision == '' {
		verbose_println('Not locking `${m.name}`: no git revision was found in `${m.install_path_fmted}`.')
		return
	}
	resolved := if m.version != '' {
		m.version
	} else {
		pseudo_version(head_commit_unix_ts(m.install_path), revision)
	}
	key := lockfile_module_key(m.requested)
	scope.entries[key] = LockedModule{
		requested: m.requested
		resolved:  resolved
		revision:  revision
		url:       m.url
	}
	verbose_println('Locked `${m.name}` at revision `${revision}`.')
}

// finish merges the entries collected during the run into the lockfile of the
// project in scope and writes it back, keeping the entries of modules the run
// did not touch. It does nothing when the run is not anchored to a project, or
// when it did not resolve any module itself.
fn (mut scope LockScope) finish() {
	if !scope.active || scope.entries.len == 0 {
		return
	}
	lock_path := lockfile_path(scope.dir)
	mut lf := LockFile{
		version: lockfile_version
		modules: map[string]LockedModule{}
	}
	if os.exists(lock_path) {
		lf = read_lockfile(scope.dir) or {
			vpm_error('failed to read `${fmt_mod_path(lock_path)}`.', details: err.msg())
			exit(1)
		}
	}
	for name, entry in scope.entries {
		lf.upsert(entry, name)
	}
	write_lockfile(scope.dir, lf) or {
		vpm_error('failed to write `${fmt_mod_path(lock_path)}`.', details: err.msg())
		exit(1)
	}
	verbose_println('Recorded ${scope.entries.len} locked module(s) in `${fmt_mod_path(lock_path)}`.')
}

// entry_for returns the lock entry recorded for the dependency string `dep`,
// when the run is anchored to a project whose lockfile lists it.
fn (scope &LockScope) entry_for(dep string) ?LockedModule {
	if !scope.active {
		return none
	}
	return scope.entries[lockfile_module_key(dep)]
}

// clone_module_source clones the source of the dependency `dep` from `url` at
// `version` into `tmp_path`. When the lock scope of the run records `dep`, the
// recorded source is cloned and its exact revision checked out, instead of
// resolving the latest HEAD. With `--locked`, a dependency that the lockfile
// does not record, or records under a different dependency string, is reported
// as an error instead of being resolved.
fn clone_module_source(vcs VCS, dep string, url string, version string, tmp_path string, mut scope LockScope) ! {
	if entry := scope.entry_for(dep) {
		if entry.requested != dep {
			if settings.is_locked {
				vpm_error('cannot install `${dep}` with `--locked`: `${lockfile_name}` in `${fmt_mod_path(scope.dir)}` records `${entry.requested}` for it.',
					details: 'Update the lockfile by running `v install` without `--locked`, or remove `${fmt_mod_path(lockfile_path(scope.dir))}` to start over.'
				)
				exit(1)
			}
			verbose_println('`${dep}` changed since it was locked as `${entry.requested}`; resolving it anew.')
		} else {
			verbose_println('Cloning `${entry.url}` at the locked revision `${entry.revision}` ...')
			vcs.clone(entry.url, '', tmp_path)!
			vcs.checkout(tmp_path, entry.revision)
			return
		}
	} else if settings.is_locked && scope.active {
		vpm_error('cannot install `${dep}` with `--locked`: `${lockfile_name}` in `${fmt_mod_path(scope.dir)}` has no entry for it.',
			details: 'Run `v install` without `--locked` once, to record it in the lockfile.'
		)
		exit(1)
	}
	vcs.clone(url, version, tmp_path)!
}

// refresh_lock_entries records the updated revisions of the modules pulled by
// `v update` in the lockfile of the project in scope, when one exists. Entries
// are matched by clone source, so only modules the project actually holds are
// refreshed, and a project without a lockfile is left alone.
fn refresh_lock_entries(results []UpdateResult) {
	dir := project_lockfile_dir()
	if dir == '' {
		return
	}
	lock_path := lockfile_path(dir)
	if !os.exists(lock_path) {
		return
	}
	mut lf := read_lockfile(dir) or {
		vpm_error('failed to read `${fmt_mod_path(lock_path)}`.', details: err.msg())
		return
	}
	mut refreshed := map[string]LockedModule{}
	for res in results {
		if !res.success || res.install_path == '' {
			continue
		}
		origin := checkout_origin_url(res.install_path)
		revision := head_revision(res.install_path)
		if origin == '' || revision == '' {
			continue
		}
		canonical_origin := normalized_clone_source(origin)
		for name, entry in lf.modules {
			canonical_entry_url := normalized_clone_source(entry.url)
			if canonical_entry_url != canonical_origin {
				continue
			}
			updated := LockedModule{
				requested: entry.requested
				resolved:  pseudo_version(head_commit_unix_ts(res.install_path), revision)
				revision:  revision
				url:       entry.url
			}
			refreshed[name] = updated
		}
	}
	if refreshed.len == 0 {
		return
	}
	for name, entry in refreshed {
		lf.upsert(entry, name)
		verbose_println('Refreshed the lock entry for `${name}` at revision `${entry.revision}`.')
	}
	write_lockfile(dir, lf) or {
		vpm_error('failed to write `${fmt_mod_path(lock_path)}`.', details: err.msg())
	}
}

// remove_lock_entries drops the entries of a removed module from the lockfile
// of the project in scope, when one exists. Entries are matched by the name
// the module was removed under, or by its clone source, since the dependency
// string of the project may spell the module differently.
fn remove_lock_entries(ident string, url string) {
	dir := project_lockfile_dir()
	if dir == '' {
		return
	}
	lock_path := lockfile_path(dir)
	if !os.exists(lock_path) {
		return
	}
	mut lf := read_lockfile(dir) or {
		vpm_error('failed to read `${fmt_mod_path(lock_path)}`.', details: err.msg())
		return
	}
	canonical_url := if url != '' { normalized_clone_source(url) } else { '' }
	mut removed := []string{}
	for name, entry in lf.modules {
		if name == ident || name == lockfile_module_key(ident) {
			removed << name
			continue
		}
		if canonical_url != '' {
			canonical_entry_url := normalized_clone_source(entry.url)
			if canonical_entry_url == canonical_url {
				removed << name
			}
		}
	}
	if removed.len == 0 {
		return
	}
	for name in removed {
		lf.remove(name)
	}
	write_lockfile(dir, lf) or {
		vpm_error('failed to write `${fmt_mod_path(lock_path)}`.', details: err.msg())
		return
	}
	println('Removed `${removed.join('`, `')}` from `${fmt_mod_path(lock_path)}`.')
}
