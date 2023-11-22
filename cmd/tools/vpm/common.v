module main

import os
import net.http
import net.urllib
import sync.pool
import v.vmod
import json
import term
import log

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

struct ModuleVpmInfo {
	// id           int
	name         string
	url          string
	vcs          string
	nr_downloads int
}

pub struct ModuleDateInfo {
	name string
mut:
	outdated bool
	exec_err bool
}

@[params]
struct ErrorOptions {
	details string
	verbose bool // is used to only output the error message if the verbose setting is enabled.
}

const (
	vexe     = os.getenv('VEXE')
	home_dir = os.home_dir()
)

fn parse_query(query []string) ([]Module, []Module) {
	mut vpm_modules, mut external_modules := []Module{}, []Module{}
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
		mod.version = version
		mod.get_installed()
		if mod.is_external {
			external_modules << mod
		} else {
			vpm_modules << mod
		}
	}
	if errors > 0 && errors == query.len {
		exit(1)
	}
	if external_modules.len > 0 {
		settings.vcs.is_executable() or {
			vpm_error(err.msg())
			exit(1)
		}
	}
	return vpm_modules, external_modules
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

fn get_mod_date_info(mut pp pool.PoolProcessor, idx int, wid int) &ModuleDateInfo {
	mut result := &ModuleDateInfo{
		name: pp.get_item[string](idx)
	}
	path := get_path_of_existing_module(result.name) or { return result }
	vcs := vcs_used_in_dir(path) or { return result }
	is_hg := vcs.cmd == 'hg'
	mut outputs := []string{}
	for step in vcs.args.outdated {
		cmd := '${vcs.cmd} ${vcs.args.path} "${path}" ${step}'
		res := os.execute(cmd)
		if res.exit_code < 0 {
			verbose_println('Error command: ${cmd}')
			verbose_println('Error details:\n${res.output}')
			result.exec_err = true
			return result
		}
		if is_hg && res.exit_code == 1 {
			result.outdated = true
			return result
		}
		outputs << res.output
	}
	// vcs.cmd == 'git'
	if !is_hg && outputs[1] != outputs[2] {
		result.outdated = true
	}
	return result
}

fn get_mod_vpm_info(name string) !ModuleVpmInfo {
	if name.len < 2 || (!name[0].is_digit() && !name[0].is_letter()) {
		return error('invalid module name `${name}`.')
	}
	mut errors := []string{}
	for url in vpm_server_urls {
		modurl := url + '/api/packages/${name}'
		verbose_println('Retrieving metadata for `${name}` from `${modurl}`...')
		r := http.get(modurl) or {
			errors << 'Http server did not respond to our request for `${modurl}`.'
			errors << 'Error details: ${err}'
			continue
		}
		if r.status_code == 404 || r.body.trim_space() == '404' {
			errors << 'Skipping module `${name}`, since `${url}` reported that `${name}` does not exist.'
			continue
		}
		if r.status_code != 200 {
			errors << 'Skipping module `${name}`, since `${url}` responded with ${r.status_code} http status code. Please try again later.'
			continue
		}
		s := r.body
		if s.len > 0 && s[0] != `{` {
			errors << 'Invalid json data'
			errors << s.trim_space().limit(100) + '...'
			continue
		}
		mod := json.decode(ModuleVpmInfo, s) or {
			errors << 'Skipping module `${name}`, since its information is not in json format.'
			continue
		}
		if '' == mod.url || '' == mod.name {
			errors << 'Skipping module `${name}`, since it is missing name or url information.'
			continue
		}
		vpm_log(@FILE_LINE, @FN, 'name: ${name}; mod: ${mod}')
		return mod
	}
	return error(errors.join_lines())
}

fn get_ident_from_url(raw_url string) !(string, string) {
	url := urllib.parse(raw_url) or { return error('failed to parse module URL `${raw_url}`.') }
	publisher, mut name := url.path.trim_left('/').rsplit_once('/') or {
		return error('failed to retrieve module name for `${url}`.')
	}
	vpm_log(@FILE_LINE, @FN, 'raw_url: ${raw_url}; publisher: ${publisher}; name: ${name}')
	name = if name.ends_with('.git') { name.replace('.git', '') } else { name }
	return publisher, name
}

fn get_name_from_url(raw_url string) !string {
	_, name := get_ident_from_url(raw_url)!
	return name
}

fn normalize_mod_path(path string) string {
	return path.replace('-', '_').to_lower()
}

fn get_outdated() ![]string {
	installed := get_installed_modules()
	mut outdated := []string{}
	mut pp := pool.new_pool_processor(callback: get_mod_date_info)
	pp.work_on_items(installed)
	for res in pp.get_results[ModuleDateInfo]() {
		if res.exec_err {
			return error('failed to check the latest commits for `${res.name}`.')
		}
		if res.outdated {
			outdated << res.name
		}
	}
	return outdated
}

fn get_all_modules() []string {
	url := get_working_server_url()
	r := http.get(url) or {
		vpm_error(err.msg(), verbose: true)
		exit(1)
	}
	if r.status_code != 200 {
		vpm_error('failed to search vpm.vlang.io.', details: 'Status code: ${r.status_code}')
		exit(1)
	}
	s := r.body
	mut read_len := 0
	mut modules := []string{}
	for read_len < s.len {
		mut start_token := "<a href='/mod"
		end_token := '</a>'
		// get the start index of the module entry
		mut start_index := s.index_after(start_token, read_len)
		if start_index == -1 {
			start_token = '<a href="/mod'
			start_index = s.index_after(start_token, read_len)
			if start_index == -1 {
				break
			}
		}
		// get the index of the end of anchor (a) opening tag
		// we use the previous start_index to make sure we are getting a module and not just a random 'a' tag
		start_token = '>'
		start_index = s.index_after(start_token, start_index) + start_token.len

		// get the index of the end of module entry
		end_index := s.index_after(end_token, start_index)
		if end_index == -1 {
			break
		}
		modules << s[start_index..end_index]
		read_len = end_index
		if read_len >= s.len {
			break
		}
	}
	return modules
}

fn get_installed_modules() []string {
	dirs := os.ls(settings.vmodules_path) or { return [] }
	mut modules := []string{}
	for dir in dirs {
		adir := os.join_path(settings.vmodules_path, dir)
		if dir in excluded_dirs || !os.is_dir(adir) {
			continue
		}
		if os.exists(os.join_path(adir, 'v.mod')) && os.exists(os.join_path(adir, '.git', 'config')) {
			// an official vlang module with a short module name, like `vsl`, `ui` or `markdown`
			modules << dir
			continue
		}
		author := dir
		mods := os.ls(adir) or { continue }
		for m in mods {
			vcs_used_in_dir(os.join_path(adir, m)) or { continue }
			modules << '${author}.${m}'
		}
	}
	return modules
}

fn get_path_of_existing_module(mod_name string) ?string {
	name := get_name_from_url(mod_name) or { mod_name.replace('-', '_').to_lower() }
	path := os.real_path(os.join_path(settings.vmodules_path, name.replace('.', os.path_separator)))
	if !os.exists(path) {
		vpm_error('failed to find `${name}` at `${path}`.')
		return none
	}
	if !os.is_dir(path) {
		vpm_error('skipping `${path}`, since it is not a directory.')
		return none
	}
	vcs_used_in_dir(path) or {
		vpm_error('skipping `${path}`, since it uses an unsupported version control system.')
		return none
	}
	return path
}

fn get_working_server_url() string {
	server_urls := if settings.server_urls.len > 0 {
		settings.server_urls
	} else {
		vpm_server_urls
	}
	for url in server_urls {
		verbose_println('Trying server url: ${url}')
		http.head(url) or {
			vpm_error('failed to connect to server url `${url}`.', details: err.msg())
			continue
		}
		return url
	}
	vpm_error('No responding vpm server found. Please check your network connectivity and try again later.')
	exit(1)
}

fn ensure_vmodules_dir_exist() {
	if !os.is_dir(settings.vmodules_path) {
		println('Creating `${settings.vmodules_path}`...')
		os.mkdir(settings.vmodules_path) or {
			vpm_error(err.msg(), verbose: true)
			exit(1)
		}
	}
}

fn (vcs &VCS) is_executable() ! {
	os.find_abs_path_of_executable(vcs.cmd) or {
		return error('VPM needs `${vcs.cmd}` to be installed.')
	}
}

fn increment_module_download_count(name string) ! {
	if settings.no_dl_count_increment {
		println('Skipping download count increment for `${name}`.')
		return
	}
	mut errors := []string{}
	for url in vpm_server_urls {
		modurl := url + '/api/packages/${name}/incr_downloads'
		r := http.post(modurl, '') or {
			errors << 'Http server did not respond to our request for `${modurl}`.'
			errors << 'Error details: ${err}'
			continue
		}
		if r.status_code != 200 {
			errors << 'Failed to increment the download count for module `${name}`, since `${url}` responded with ${r.status_code} http status code. Please try again later.'
			continue
		}
		return
	}
	return error(errors.join_lines())
}

fn get_manifest(path string) ?vmod.Manifest {
	return vmod.from_file(os.join_path(path, 'v.mod')) or { return none }
}

fn resolve_dependencies(manifest ?vmod.Manifest, modules []string) {
	mod := manifest or { return }
	// Filter out modules that are both contained in the input query and listed as
	// dependencies in the mod file of the module that is supposed to be installed.
	deps := mod.dependencies.filter(it !in modules)
	if deps.len > 0 {
		println('Resolving ${deps.len} dependencies for module `${mod.name}`...')
		verbose_println('Found dependencies: ${deps}')
		vpm_install(deps)
	}
}

fn vcs_used_in_dir(dir string) ?VCS {
	for vcs in supported_vcs.values() {
		if os.is_dir(os.real_path(os.join_path(dir, vcs.dir))) {
			return vcs
		}
	}
	return none
}

fn verbose_println(msg string) {
	if settings.is_verbose {
		println(msg)
	}
}

fn vpm_log(line string, func string, msg string) {
	log.debug('${line} | (${func}) ${msg}')
}

fn vpm_error(msg string, opts ErrorOptions) {
	if opts.verbose && !settings.is_verbose {
		return
	}
	eprintln(term.ecolorize(term.red, 'error: ') + msg)
	if opts.details.len > 0 && settings.is_verbose {
		eprint(term.ecolorize(term.blue, 'details: '))
		padding := ' '.repeat('details: '.len)
		for i, line in opts.details.split_into_lines() {
			if i > 0 {
				eprint(padding)
			}
			eprintln(term.ecolorize(term.blue, line))
		}
	}
}

fn vpm_warn(msg string, opts ErrorOptions) {
	eprintln(term.ecolorize(term.yellow, 'warning: ') + msg)
	if opts.details.len > 0 {
		eprint(term.ecolorize(term.blue, 'details: '))
		padding := ' '.repeat('details: '.len)
		for i, line in opts.details.split_into_lines() {
			if i > 0 {
				eprint(padding)
			}
			eprintln(term.ecolorize(term.blue, line))
		}
	}
}

// Formatted version of the vmodules install path. E.g. `/home/user/.vmodules` -> `~/.vmodules`
fn fmt_mod_path(path string) string {
	if !path.contains(home_dir) {
		return path
	}
	return $if windows {
		path.replace(home_dir, '%USERPROFILE%')
	} $else {
		path.replace(home_dir, '~')
	}
}
