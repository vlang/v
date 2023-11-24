module main

import os
import net.http
import v.vmod

struct Module {
mut:
	name               string
	url                string
	version            string // specifies the requested version.
	install_path       string
	install_path_fmted string
	installed_version  string
	is_installed       bool
	is_external        bool
	vcs                ?VCS
	manifest           vmod.Manifest
}

fn parse_query(query []string) []Module {
	mut modules := []Module{}
	mut checked_settings_vcs := false
	mut errors := 0
	is_git_setting := settings.vcs.cmd == 'git'
	for m in query {
		ident, version := m.rsplit_once('@') or { m, '' }
		println('Scanning `${ident}`...')
		is_http := if ident.starts_with('http://') {
			vpm_warn('installing `${ident}` via http.',
				details: 'Support for `http` is deprecated, use `https` to ensure future compatibility.'
			)
			true
		} else {
			false
		}
		mut mod := if is_http || ident.starts_with('https://') {
			// External module. The idenifier is an URL.
			publisher, name := get_ident_from_url(ident) or {
				vpm_error(err.msg())
				errors++
				continue
			}
			// Verify VCS. Only needed once for external modules.
			if !checked_settings_vcs {
				checked_settings_vcs = true
				settings.vcs.is_executable() or {
					vpm_error(err.msg())
					exit(1)
				}
			}
			// Fetch manifest.
			manifest := fetch_manifest(name, ident, version, is_git_setting) or {
				vpm_error('failed to find `v.mod` for `${ident}${at_version(version)}`.',
					details: err.msg()
				)
				errors++
				continue
			}
			// Resolve path.
			base := if is_http { publisher } else { '' }
			install_path := normalize_mod_path(os.real_path(os.join_path(settings.vmodules_path,
				base, manifest.name)))
			Module{
				name: manifest.name
				url: ident
				install_path: install_path
				is_external: true
				manifest: manifest
			}
		} else {
			// VPM registered module.
			info := get_mod_vpm_info(ident) or {
				vpm_error('failed to retrieve metadata for `${ident}`.', details: err.msg())
				errors++
				continue
			}
			// Verify VCS.
			mut is_git_module := true
			vcs := if info.vcs != '' {
				info_vcs := supported_vcs[info.vcs] or {
					vpm_error('skipping `${info.name}`, since it uses an unsupported version control system `${info.vcs}`.')
					errors++
					continue
				}
				is_git_module = info.vcs == 'git'
				if !is_git_module && version != '' {
					vpm_error('skipping `${info.name}`, version installs are currently only supported for projects using `git`.')
					errors++
					continue
				}
				info_vcs
			} else {
				supported_vcs['git']
			}
			vcs.is_executable() or {
				vpm_error(err.msg())
				errors++
				continue
			}
			// Fetch manifest.
			manifest := fetch_manifest(info.name, info.url, version, is_git_module) or {
				// Add link with issue template requesting to add a manifest.
				mut details := ''
				if resp := http.head('${info.url}/issues/new') {
					if resp.status_code == 200 {
						issue_tmpl_url := '${info.url}/issues/new?title=Missing%20Manifest&body=${info.name}%20is%20missing%20a%20manifest,%20please%20consider%20adding%20a%20v.mod%20file%20with%20the%20modules%20metadata.'
						details = 'Help to ensure future-compatibility by adding a `v.mod` file or opening an issue at:\n`${issue_tmpl_url}`'
					}
				}
				vpm_warn('`${info.name}` is missing a manifest file.', details: details)
				vpm_log(@FILE_LINE, @FN, 'vpm manifest detection error: ${err}')
				vmod.Manifest{}
			}
			// Resolve path.
			ident_as_path := info.name.replace('.', os.path_separator)
			install_path := normalize_mod_path(os.real_path(os.join_path(settings.vmodules_path,
				ident_as_path)))
			Module{
				name: info.name
				url: info.url
				vcs: vcs
				install_path: install_path
				manifest: manifest
			}
		}
		mod.install_path_fmted = fmt_mod_path(mod.install_path)
		mod.version = version
		mod.get_installed()
		modules << mod
	}
	if errors > 0 && errors == query.len {
		exit(1)
	}
	return modules
}

// TODO: add unit test
fn (mut m Module) get_installed() {
	refs := os.execute_opt('git ls-remote --refs ${m.install_path}') or { return }
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

fn fetch_manifest(name string, url string, version string, is_git bool) !vmod.Manifest {
	if !is_git {
		// TODO: fetch manifest for mercurial repositories
		return vmod.Manifest{
			name: name
		}
	}
	v := if version != '' {
		version
	} else {
		head_branch := os.execute_opt('git ls-remote --symref ${url} HEAD') or {
			return error('failed to find git HEAD. ${err}')
		}
		head_branch.output.all_after_last('/').all_before(' ').all_before('\t')
	}
	url_ := if url.ends_with('.git') { url.replace('.git', '') } else { url }
	// Scan both URLS. E.g.:
	// https://github.com/publisher/module/raw/v0.7.0/v.mod
	// https://gitlab.com/publisher/module/-/raw/main/v.mod
	raw_paths := ['raw/', '/-/raw/']
	for i, raw_p in raw_paths {
		manifest_url := '${url_}/${raw_p}/${v}/v.mod'
		vpm_log(@FILE_LINE, @FN, 'manifest_url ${i}: ${manifest_url}')
		raw_manifest_resp := http.get(manifest_url) or { continue }
		if raw_manifest_resp.status_code != 200 {
			return error('unsuccessful response status `${raw_manifest_resp.status_code}`.')
		}
		return vmod.decode(raw_manifest_resp.body) or {
			return error('failed to decode manifest `${raw_manifest_resp.body}`. ${err}')
		}
	}
	return error('failed to retrieve manifest.')
}
