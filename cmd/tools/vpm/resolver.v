module main

import crypto.sha256
import os
import rand
import v.vmod

struct VersionChoice {
	version   string
	revision  string
	automatic bool
}

struct Requirement {
	raw       string
	chain     []string
	root      bool
	requiring string
}

// Resolver owns disposable checkouts. Only a successful, complete assignment is
// handed to install_modules; failed choices never change installed modules.
struct Resolver {
mut:
	parser      Parser
	aliases     map[string]string
	sources     map[string]Module
	tags        map[string][]string
	retractions map[string][]string
	candidates  map[string]Module
	exact_refs  map[string][]string
	paths       []string
	prefer_lock bool
	precise     map[string]string
	failure     string
	overrides   []Override
	namespace   string
}

// resolver_tmp_namespace keeps only random ULID characters in the short namespace.
fn resolver_tmp_namespace(id string) string {
	return os.join_path('resolver', id[id.len - tmp_name_length..])
}

fn resolve_module_query(query []string, mut selector VpmInstallServerSelector, mut scope LockScope, prefer_lock bool, precise map[string]string) ![]Module {
	// The namespace separates this resolver's temp clones from any other vpm
	// process's, so it only has to be unique among the ones running now. A
	// full ULID is 26 characters, and that length sits directly in the temp
	// path a clone is checked out into; on Windows, with `.git/objects` and a
	// long module name underneath, it is enough to push past `MAX_PATH`. The
	// same length is used for the other short components below.
	namespace := resolver_tmp_namespace(rand.ulid())
	mut overrides := []Override{}
	project := vmod.get_cache().get_by_folder(os.getwd())
	root_file := if scope.active { os.join_path(scope.dir, 'v.mod') } else { project.vmod_file }
	mut root_name := 'command line'
	if root_file != '' {
		manifest := vmod.from_file(root_file)!
		root_name = manifest.name
		overrides = parse_overrides(manifest.unknown['dependency_overrides'] or { []string{} })!
	}

	mut r := Resolver{
		namespace:   namespace
		parser:      Parser{ temporary_namespace: os.join_path(namespace, 'sources'), shallow: true, quiet: settings.is_outdated, probe: true }
		prefer_lock: prefer_lock
		overrides:   overrides
		precise:     precise
	}
	mut pending := []Requirement{}
	root := if scope.active { os.file_name(scope.dir) } else { 'command line' }
	for raw in query {
		pending << Requirement{ raw: raw, chain: [root], root: true, requiring: root_name }
	}
	selected := r.solve(pending, map[string]Module{}, map[string][]Requirement{}, mut selector, mut scope) or {
		r.cleanup([]Module{})
		return error(r.failure)
	}
	modules := selected.values().sorted(a.install_path < b.install_path)
	validate_resolved_destinations(modules) or {
		r.cleanup([]Module{})
		return err
	}
	r.cleanup(modules)
	return modules
}

fn (mut r Resolver) cleanup(keep []Module) {
	kept := keep.map(it.tmp_path)
	for path in r.paths {
		if path !in kept {
			rmdir_all(path) or {}
		}
	}
}

// source discovers repository identity without following the current HEAD's
// dependencies: a release can have a different graph from the default branch.
fn (mut r Resolver) source(raw string, mut selector VpmInstallServerSelector) !string {
	ident := lockfile_module_key(raw)
	if id := r.aliases[ident] {
		return id
	}
	for id, m in r.sources {
		if normalized_clone_source(m.url) == normalized_clone_source(ident) {
			r.aliases[ident] = id
			return id
		}
	}
	mut discovery := LockScope{}
	before := r.parser.errors
	discovery_query := if r.overrides.len > 0 || is_version_range(requirement_version(raw)) || is_git_commit_hash(requirement_version(raw)) {
		ident
	} else {
		raw
	}
	r.parser.parse_module(discovery_query, mut selector, mut discovery)
	for m in r.parser.modules.values() {
		if m.tmp_path !in r.paths {
			r.paths << m.tmp_path
		}
		if m.requested != discovery_query {
			continue
		}
		id := (m.vcs or { settings.vcs }).str() + ':' + normalized_clone_source(m.url)
		r.aliases[ident] = id
		if id !in r.sources {
			r.sources[id] = m
			r.candidates[id + '\0' + m.version + '\0'] = m
		}
		return id
	}
	if r.parser.errors > before && r.parser.last_error != '' {
		return error(r.parser.last_error)
	}
	return error('cannot discover `${raw}`')
}

fn (mut r Resolver) version_tags(id string) ![]string {
	if tags := r.tags[id] {
		return tags
	}
	m := r.sources[id]
	if (m.vcs or { settings.vcs }) != .git {
		r.tags[id] = []string{}
		return []string{}
	}
	tags := remote_version_tags(m.url)!
	r.tags[id] = tags
	return tags
}

fn (mut r Resolver) version_retractions(id string) ![]string {
	if ranges := r.retractions[id] {
		return ranges
	}
	ranges := latest_version_retractions(r.sources[id].url, r.version_tags(id)!)!
	r.retractions[id] = ranges
	return ranges
}

fn remote_version_tags(url string) ![]string {
	if url.contains_any('\0\r\n') || url.starts_with('-') {
		return error('invalid repository source')
	}
	res := os.exec_opt(['git', 'ls-remote', '--tags', '--refs', '--', url])!
	mut tags := []string{}
	for line in res.output.split_into_lines() {
		fields := line.split('\t')
		if fields.len == 2 && fields[1].starts_with('refs/tags/') {
			tag := fields[1].trim_string_left('refs/tags/')
			version_tag(tag) or { continue }
			tags << tag
		}
	}
	return sorted_version_tags(tags)
}

fn sorted_version_tags(tags []string) []string {
	mut remaining := tags.clone()
	remaining.sort()
	mut result := []string{}
	for remaining.len > 0 {
		mut best := 0
		mut highest := version_tag(remaining[0]) or {
			remaining.delete(0)
			continue
		}
		for i, tag in remaining {
			ver := version_tag(tag) or { continue }
			if ver > highest {
				best = i
				highest = ver
			}
		}
		result << remaining[best]
		remaining.delete(best)
	}
	return result
}

fn requirement_version(raw string) string {
	ident := lockfile_module_key(raw)
	return if ident == raw { '' } else { raw[ident.len + 1..] }
}

fn (mut r Resolver) candidate(id string, version string, revision string) !Module {
	key := id + '\0' + version + '\0' + revision
	if m := r.candidates[key] {
		return m
	}
	mut m := r.sources[id]
	// The component is a digest of the candidate key, cut to a short prefix for
	// the same reason as `version_tmp_name`: a full SHA-256 as one path
	// component pushes the temp path over `MAX_PATH` on Windows. The 48 bits
	// that remain separate the candidates of one resolution comfortably.
	path := get_tmp_path(settings.tmp_path, os.join_path(r.namespace,
		sha256.hexhash(key)[0..tmp_name_length]))!
	r.paths << path
	vcs := m.vcs or { settings.vcs }
	clone_ref := if !is_git_commit_hash(version) && (revision == '' || (version != '' && version == r.sources[id].version)) {
		version
	} else {
		''
	}
	vcs.clone(m.url, clone_ref, path)!
	if revision != '' || is_git_commit_hash(version) {
		vcs.checkout(path, if revision == '' { version } else { revision })!
	}
	manifest := vmod.from_file(os.join_path(path, 'v.mod')) or {
		if m.is_external {
			return error('`${m.url}@${version}` has no valid v.mod: ${err.msg()}')
		}
		vmod.Manifest{}
	}
	if m.is_external && direct_install_mod_path('', manifest.name) != direct_install_mod_path('', m.manifest.name) {
		return error('`${m.url}@${version}` changes its module name from `${m.manifest.name}` to `${manifest.name}`')
	}
	m.tmp_path = path
	m.manifest = manifest
	m.version = version
	m.is_installed = false
	m.installed_version = ''
	r.candidates[key] = m
	return m
}

fn module_satisfies(m Module, constraint string) bool {
	if constraint == '' {
		return true
	}
	if is_version_range(constraint) {
		if tag_satisfies_range(m.version, constraint) { return true }
		return checkout_satisfying_tag(m.tmp_path, constraint) != ''
	}
	if m.version == constraint {
		return true
	}
	// Two exact refs may denote the same commit (including a range's selected tag).
	if (m.vcs or { settings.vcs }) == .git {
		ref := os.exec(['git', '-C', m.tmp_path, 'rev-parse', '--verify', '--end-of-options',
			constraint + '^{commit}'])
		return ref.exit_code == 0 && ref.output.trim_space() == head_revision(m.tmp_path)
	}
	return false
}

fn checkout_version_tag(path string) string {
	res := os.exec(['git', '-C', path, 'tag', '--points-at', 'HEAD'])
	if res.exit_code != 0 {
		return ''
	}
	tags := sorted_version_tags(res.output.trim_space().split_into_lines())
	return if tags.len == 0 { '' } else { tags[0] }
}

fn clone_selected_modules(selected map[string]Module) map[string]Module {
	mut result := map[string]Module{}
	for id, m in selected {
		result[id] = Module{ ...m, requested_aliases: m.requested_aliases.clone() }
	}
	return result
}

fn (mut r Resolver) check_locked_root_request(req Requirement, selected map[string]Module, scope &LockScope) ! {
	if settings.is_locked && scope.active && req.root && r.aliases[lockfile_module_key(req.raw)] !in selected {
		if entry := scope.entry_for(req.raw) {
			if entry.requested != req.raw {
				r.failure = 'cannot install `${req.raw}` with `--locked`: ${lockfile_name} ${lock_mismatch(entry, req.raw, entry.url)}.'
				return error(r.failure)
			}
		}
	}
}

fn (mut r Resolver) solve(pending []Requirement, selected map[string]Module, requirements map[string][]Requirement, mut selector VpmInstallServerSelector, mut scope LockScope) !map[string]Module {
	if pending.len == 0 {
		return selected
	}
	mut req := pending[0]
	req = Requirement{
		...req
		raw: overridden_request_for_module(req.raw,
			[lockfile_module_key(req.raw)], req.requiring, r.overrides)
	}
	if requirement_version(req.raw) == '-' {
		return r.solve(pending[1..], selected, requirements, mut selector, mut scope)
	}
	if r.overrides.len == 0 { r.check_locked_root_request(req, selected, scope)! }
	id := r.source(req.raw, mut selector) or {
		r.failure = '${err.msg()}\nrequired by ${req.chain.join(' -> ')} -> ${req.raw}'
		return err
	}
	req = Requirement{
		...req
		raw: overridden_request_for_module(req.raw,
			[lockfile_module_key(req.raw), r.sources[id].name], req.requiring, r.overrides)
	}
	if requirement_version(req.raw) == '-' {
		return r.solve(pending[1..], selected, requirements, mut selector, mut scope)
	}
	r.check_locked_root_request(req, selected, scope)!
	constraint := requirement_version(req.raw)
	if constraint != '' && !is_version_range(constraint) && constraint !in r.exact_refs[id] {
		r.exact_refs[id] << constraint
	}
	mut known := requirements.clone()
	mut for_module := known[id].clone()
	for_module << req
	known[id] = for_module
	if selected_module := selected[id] {
		mut m := Module{ ...selected_module, requested_aliases: selected_module.requested_aliases.clone() }
		if module_satisfies(m, constraint) {
			if req.raw !in m.requested_aliases {
				m.requested_aliases << req.raw
			}
			mut next_selected := clone_selected_modules(selected)
			next_selected[id] = m
			return r.solve(pending[1..], next_selected, known, mut selector, mut scope)
		}
		r.conflict(id, for_module)
		return error(r.failure)
	}
	mut choices := []VersionChoice{}
	mut locked := false
	if r.prefer_lock && !r.wants_update(req.raw, id) {
		if entry := scope.entry_for(req.raw) {
			mismatch := lock_mismatch(entry, req.raw, r.sources[id].url)
			if req.root && mismatch != '' {
				if settings.is_locked {
					r.failure = 'cannot install `${req.raw}` with `--locked`: ${lockfile_name} ${mismatch}.'
					return error(r.failure)
				}
				verbose_println('`${req.raw}` changed since it was locked (${lockfile_name} ${mismatch}); resolving it anew.')
			}
			if normalized_clone_source(entry.url) == normalized_clone_source(r.sources[id].url)
				&& (!req.root || entry.requested == req.raw)
				&& (constraint == '' || entry.resolved == constraint
					|| (is_version_range(constraint) && tag_satisfies_range(entry.resolved, constraint))) {
				choices << VersionChoice{
					version:  if constraint == '' {
						''
					} else {
						entry.resolved
					}
					revision: entry.revision
				}
				locked = true
			}
		}
	}
	if settings.is_locked && scope.active && !locked {
		r.failure = 'cannot resolve `${req.raw}` with `--locked`: ${lockfile_name} records no matching, satisfying entry\nrequired by ${req.chain.join(' -> ')}'
		return error(r.failure)
	}
	forced := r.precise[lockfile_module_key(req.raw)] or { r.precise[r.sources[id].name] or { '' } }
	if forced != '' {
		mut ref := forced
		if version := version_tag(forced) {
			for tag in r.version_tags(id) or { []string{} } {
				candidate_version := version_tag(tag) or { continue }
				if tag == forced {
					ref = tag
					break
				}
				if candidate_version.str() == version.str() { ref = tag }
			}
		}
		choices = [VersionChoice{ version: ref }]
	} else if !(settings.is_locked && scope.active) {
		if constraint == '' || !is_version_range(constraint) {
			choices << VersionChoice{ version: constraint }
		}
		if constraint == '' || is_version_range(constraint) {
			tags := r.version_tags(id) or {
				r.failure = 'cannot list versions for `${req.raw}`: ${err.msg()}'
				return err
			}
			for tag in tags {
				allowed := release_tag_allowed(r.sources[id].url, tag) or {
					r.failure = 'cannot apply release policy for `${req.raw}`: ${err.msg()}'
					return err
				}
				if !allowed { continue }
				if tag_satisfies_range(tag, if constraint == '' { '*' } else { constraint }) {
					choices << VersionChoice{ version: tag, automatic: true }
				}
			}
		}
	}
	if choices.len == 0 {
		r.conflict(id, for_module)
		return error(r.failure)
	}
	mut tried := map[string]bool{}
	mut choice_index := 0
	mut attempted := false
	for choice_index < choices.len {
		choice := choices[choice_index]
		choice_index++
		if choice.automatic {
			retractions := r.version_retractions(id) or {
				r.failure = 'cannot read retractions for `${req.raw}`: ${err.msg()}'
				return err
			}
			if version_is_retracted(choice.version, retractions) { continue }
		}
		key := choice.version + '\0' + choice.revision
		if key in tried {
			continue
		}
		tried[key] = true
		attempted = true
		mut m := r.candidate(id, choice.version, choice.revision) or {
			r.failure = 'failed to install `${req.raw}` at `${choice.version}`: ${err.msg()}\nrequired by ${req.chain.join(' -> ')}'
			if choice.revision != '' { return err }
			continue
		}
		check_min_v(m.manifest, m.name) or {
			r.failure = err.msg()
			if choice.revision != '' { return err }
			continue
		}
		if choice.revision != '' {
			if entry := scope.entry_for(req.raw) {
				actual_hash := if entry.hash == '' {
					''
				} else {
					dir_sha256(m.tmp_path) or {
						r.failure = 'cannot verify locked `${req.raw}`: ${err.msg()}'
						return err
					}
				}
				if entry.hash != '' && actual_hash != entry.hash {
					r.failure = 'content hash mismatch for `${req.raw}` at its locked revision'
					return error(r.failure)
				}
			}
		}
		if !module_satisfies(m, constraint) {
			r.conflict(id, for_module)
			continue
		}
		if is_git_commit_hash(m.version) && is_version_range(constraint) {
			m.version = checkout_satisfying_tag(m.tmp_path, constraint)
		}
		m.requested = req.raw
		m.requested_aliases = [req.raw]
		m.version_range = if is_version_range(constraint) { constraint } else { '' }
		m.is_resolution_update = !r.prefer_lock || r.precise.len > 0
		m.is_resolved = true
		m.get_installed()
		mut next_selected := clone_selected_modules(selected)
		next_selected[id] = m
		mut next := pending[1..].clone()
		mut chain := req.chain.clone()
		chain << m.name + at_version(m.version)
		for dep in m.manifest.dependencies {
			next << Requirement{ raw: dep, chain: chain, requiring: m.name }
		}
		if result := r.solve(next, next_selected, known, mut selector, mut scope) {
			return result
		}
		// An unconstrained choice can meet an exact ref only after another
		// candidate's manifest introduces it. Retry those discovered refs too.
		if forced == '' && !(settings.is_locked && scope.active) {
			for exact in r.exact_refs[id] {
				if !choices.any(it.version == exact && it.revision == '') {
					choices << VersionChoice{ version: exact }
				}
			}
		}
	}
	if !attempted { r.conflict(id, for_module) }
	return error(r.failure)
}

fn (mut r Resolver) conflict(id string, requirements []Requirement) {
	m := r.sources[id]
	mut lines := ['no semantic-version tag satisfies all requirements for `${m.name}` (multiple requirements may conflict):']
	for req in requirements {
		lines << '  ${req.chain.join(' -> ')} -> ${req.raw}'
	}
	r.failure = lines.join('\n')
}

fn validate_resolved_destinations(modules []Module) ! {
	mut seen := map[string]Module{}
	for m in modules {
		mut destination := os.norm_path(real_path_with_missing_suffix(m.install_path))
		$if windows {
			destination = destination.to_lower()
		}
		if previous := seen[destination] {
			return error('different repositories target `${fmt_mod_path(destination)}`: `${previous.requested}` and `${m.requested}`')
		}
		seen[destination] = m
	}
}

fn (r &Resolver) wants_update(raw string, id string) bool {
	return lockfile_module_key(raw) in r.precise || r.sources[id].name in r.precise
}

fn is_git_commit_hash(value string) bool {
	return value.len >= 7 && value.len <= 40 && value.bytes().all((it >= `0` && it <= `9`) || (it >= `a` && it <= `f`) || (it >= `A` && it <= `F`))
}

fn checkout_satisfying_tag(path string, constraint string) string {
	res := os.exec(['git', '-C', path, 'tag', '--points-at', 'HEAD'])
	if res.exit_code != 0 { return '' }
	return select_version_tag(res.output.trim_space().split_into_lines(), constraint) or { '' }
}
