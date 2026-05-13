module main

import os
import net.http
import v.vmod

struct Module {
mut:
	name               string
	url                string
	version            string // specifies the requested version.
	tmp_path           string
	install_path       string
	install_path_fmted string
	installed_version  string
	is_installed       bool
	is_external        bool
	vcs                ?VCS
	manifest           vmod.Manifest
}

struct Parser {
mut:
	modules              map[string]Module
	checked_settings_vcs bool
	search_modules       []string
	search_loaded        bool
	errors               int
}

enum ModuleKind {
	registered
	https
	http
	ssh
	local
}

fn parse_query(query []string, mut selector VpmInstallServerSelector) []Module {
	mut p := Parser{}
	for m in query {
		p.parse_module(m, mut selector)
	}
	if p.errors > 0 && p.errors == query.len {
		exit(1)
	}
	return p.modules.values()
}

fn (mut p Parser) lookup_registered_name_for_url(manifest_name string, ident string, mut selector VpmInstallServerSelector) ?string {
	if manifest_name == '' || manifest_name.contains('.') {
		return none
	}
	expected_url := normalize_repo_lookup_url(ident) or { return none }
	if !p.search_loaded {
		p.search_modules = get_all_modules_for_search_with_selector(mut selector) or {
			vpm_log(@FILE_LINE, @FN, 'failed to load the VPM search index for `${ident}`: ${err}')
			p.search_loaded = true
			return none
		}
		p.search_loaded = true
	}
	target_name := normalize_mod_path(manifest_name)
	for registered_name in p.search_modules {
		if normalize_mod_path(registered_name.all_after_last('.')) != target_name {
			continue
		}
		info := get_mod_vpm_info_with_selector(registered_name, mut selector) or {
			vpm_log(@FILE_LINE, @FN, 'failed to retrieve metadata for `${registered_name}`: ${err}')
			continue
		}
		registered_url := normalize_repo_lookup_url(info.url) or { continue }
		if registered_url == expected_url {
			return info.name
		}
	}
	return none
}

fn (mut p Parser) parse_module(m string, mut selector VpmInstallServerSelector) {
	kind := match true {
		m.starts_with('https://') { ModuleKind.https }
		m.starts_with('git@') { ModuleKind.ssh }
		m.starts_with('http://') { ModuleKind.http }
		is_local_repository(m) { ModuleKind.local }
		else { ModuleKind.registered }
	}

	ident, version := if kind == .ssh {
		if m.count('@') > 1 {
			m.all_before_last('@'), m.all_after_last('@')
		} else {
			m, ''
		}
	} else {
		m.rsplit_once('@') or { m, '' }
	}
	key := match kind {
		.registered { m }
		.ssh { ident.replace(':', '/') + at_version(version) }
		else { ident.all_after('//').trim_string_right('.git') + at_version(version) }
	}

	if key in p.modules {
		return
	}
	println('Scanning `${m}`...')
	mut mod := if kind != ModuleKind.registered {
		// External module. The identifier is an URL.
		if kind == .http {
			vpm_warn('installing `${ident}` via http.',
				details: 'Support for `http` is deprecated, use `https` to ensure future compatibility.'
			)
		}
		publisher, name := get_ident_from_url(if kind == .ssh {
			'https://' + ident['git@'.len..].replace(':', '/')
		} else {
			ident
		}) or {
			vpm_error(err.msg())
			p.errors++
			return
		}
		// Verify VCS. Only needed once for external modules.
		if !p.checked_settings_vcs {
			p.checked_settings_vcs = true
			settings.vcs.is_executable() or {
				vpm_error(err.msg())
				exit(1)
			}
		}
		tmp_path := get_tmp_path(os.join_path(publisher, name, version)) or {
			vpm_error('failed to get temporary directory for `${ident}`.', details: err.msg())
			p.errors++
			return
		}
		settings.vcs.clone(ident, version, tmp_path) or {
			vpm_error('failed to install `${ident}`.', details: err.msg())
			p.errors++
			return
		}
		manifest := get_manifest(tmp_path) or {
			vpm_error('failed to find `v.mod` for `${ident}${at_version(version)}`.',
				details: err.msg()
			)
			rmdir_all(tmp_path) or {}
			p.errors++
			return
		}
		// Reuse the registered VPM name when a direct VCS URL points to the same repository.
		registered_name := if kind in [.https, .ssh] {
			p.lookup_registered_name_for_url(manifest.name, ident, mut selector) or { '' }
		} else {
			''
		}
		final_name := if registered_name != '' { registered_name } else { manifest.name }
		mod_path := if registered_name != '' {
			normalize_mod_path(final_name.replace('.', os.path_separator))
		} else {
			normalize_mod_path(os.join_path(if kind == .http { publisher } else { '' },
				manifest.name))
		}
		Module{
			name:         final_name
			url:          ident
			version:      version
			install_path: os.real_path(os.join_path(settings.vmodules_path, mod_path))
			is_external:  true
			tmp_path:     tmp_path
			manifest:     manifest
		}
	} else {
		// VPM registered module.
		info := get_mod_vpm_info_with_selector(ident, mut selector) or {
			vpm_error('failed to retrieve metadata for `${ident}`.', details: err.msg())
			p.errors++
			return
		}
		// Verify VCS.
		vcs := if info.vcs != '' {
			info_vcs := vcs_from_str(info.vcs) or {
				vpm_error('skipping `${info.name}`, since it uses an unsupported version control system `${info.vcs}`.')
				p.errors++
				return
			}
			info_vcs
		} else {
			VCS.git
		}
		vcs.is_executable() or {
			vpm_error(err.msg())
			p.errors++
			return
		}
		mod_path := normalize_mod_path(info.name.replace('.', os.path_separator))
		tmp_path := get_tmp_path(os.join_path(mod_path, version)) or {
			vpm_error('failed to get temporary directory for `${ident}`.', details: err.msg())
			p.errors++
			return
		}
		vcs.clone(info.url, version, tmp_path) or {
			vpm_error('failed to install `${ident}`.', details: err.msg())
			p.errors++
			return
		}
		manifest := get_manifest(tmp_path) or {
			// Add link with issue template requesting to add a manifest.
			mut details := ''
			new_issue_url := '${info.url}/issues/new'
			verbose_println_more(@FILE_LINE, @FN, 'making a HEAD request to: ${new_issue_url} ...')
			if resp := http.head(new_issue_url) {
				if resp.status_code == 200 {
					issue_tmpl_url := '${info.url}/issues/new?title=Missing%20Manifest&body=${info.name}%20is%20missing%20a%20manifest,%20please%20consider%20adding%20a%20v.mod%20file%20with%20the%20modules%20metadata.'
					details = 'Please help us ensure future-compatibility, by adding a `v.mod` file or opening an issue at:\n`${issue_tmpl_url}`'
				}
			}
			vpm_warn('`${info.name}` is missing a manifest file.', details: details)
			vpm_log(@FILE_LINE, @FN, 'vpm manifest detection error: ${err}')
			vmod.Manifest{}
		}
		Module{
			name:         info.name
			url:          info.url
			version:      version
			vcs:          vcs
			install_path: os.real_path(os.join_path(settings.vmodules_path, mod_path))
			tmp_path:     tmp_path
			manifest:     manifest
		}
	}
	mod.install_path_fmted = fmt_mod_path(mod.install_path)
	mod.get_installed()
	p.modules[key] = mod
	if mod.manifest.dependencies.len > 0 {
		verbose_println('Found ${mod.manifest.dependencies.len} dependencies for `${mod.name}`: ${mod.manifest.dependencies}.')
		for d in mod.manifest.dependencies {
			p.parse_module(d, mut selector)
		}
	}
}

fn is_local_repository(query string) bool {
	if query.starts_with('file://') {
		return true
	}
	mut path_candidates := [query]
	if !query.starts_with('git@') {
		ident, _ := query.rsplit_once('@') or { query, '' }
		if ident != query {
			path_candidates << ident
		}
	}
	for candidate in path_candidates {
		path := os.expand_tilde_to_home(candidate)
		if os.is_abs_path(path) || path.starts_with('./') || path.starts_with('../')
			|| path.starts_with('~/') {
			return true
		}
		if os.exists(path) {
			return true
		}
	}
	return false
}

fn (mut m Module) get_installed() {
	refs := os.execute_opt('git ls-remote --refs ${m.install_path}') or { return }
	vpm_log(@FILE_LINE, @FN, 'refs: ${refs}')
	m.is_installed = true
	// In case the head just temporarily matches a tag, make sure that there
	// really is a version installation before adding it as `installed_version`.
	// NOTE: can be refined for branch installations. E.g., for `sdl`.
	if refs.output.contains('refs/tags/') {
		tag := refs.output.all_after_last('refs/tags/').all_before('\n').trim_space()
		head := if refs.output.contains('refs/heads/') {
			refs.output.all_after_last('refs/heads/').all_before('\n').trim_space()
		} else {
			tag
		}
		vpm_log(@FILE_LINE, @FN, 'head: ${head}, tag: ${tag}')
		if tag == head {
			m.installed_version = tag
		}
	}
}

fn get_tmp_path(relative_path string) !string {
	tmp_path := os.real_path(os.join_path(settings.tmp_path, relative_path))
	if os.exists(tmp_path) {
		// It's unlikely that the tmp_path already exists, but it might
		// occur if vpm was canceled during an installation or update.
		rmdir_all(tmp_path)!
	}
	return tmp_path
}
