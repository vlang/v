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
	// Fields determined by the url or the info received from the VPM API.
	name string
	url  string
	vcs  string
	// Fields based on preference / environment.
	version            string // specifies the requested version.
	install_path       string
	install_path_fmted string
	is_installed       bool
	is_external        bool
	installed_version  string
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

[params]
struct ErrorOptions {
	details string
	verbose bool // is used to only output the error message if the verbose setting is enabled.
}

const home_dir = os.home_dir()

fn parse_query(query []string) ([]Module, []Module) {
	mut vpm_modules, mut external_modules := []Module{}, []Module{}
	mut errors := 0
	for m in query {
		ident, version := m.rsplit_once('@') or { m, '' }
		mut mod := if ident.starts_with('https://') {
			name := get_name_from_url(ident) or {
				vpm_error(err.msg())
				errors++
				continue
			}
			install_path := os.real_path(os.join_path(settings.vmodules_path, name))
			if !has_vmod(ident, install_path) {
				errors++
				continue
			}
			Module{
				name: name
				url: ident
				install_path: install_path
				install_path_fmted: fmt_mod_path(install_path)
				is_external: true
			}
		} else {
			info := get_mod_vpm_info(ident) or {
				vpm_error('failed to retrieve metadata for `${ident}`.', details: err.msg())
				errors++
				continue
			}
			name_normalized := info.name.replace('-', '_').to_lower()
			name_as_path := name_normalized.replace('.', os.path_separator)
			install_path := os.real_path(os.join_path(settings.vmodules_path, name_as_path))
			Module{
				name: info.name
				url: info.url
				vcs: info.vcs
				install_path: install_path
				install_path_fmted: fmt_mod_path(install_path)
			}
		}
		mod.version = version
		if v := os.execute_opt('git ls-remote --tags ${mod.install_path}') {
			mod.is_installed = true
			mod.installed_version = v.output.all_after_last('/').trim_space()
		}
		if mod.is_external {
			external_modules << mod
		} else {
			vpm_modules << mod
		}
	}
	if errors > 0 && errors == query.len {
		exit(1)
	}
	return vpm_modules, external_modules
}

fn has_vmod(url string, install_path string) bool {
	if os.exists((os.join_path(install_path, 'v.mod'))) {
		// Safe time fetchting the repo when the module is already installed and has a `v.mod`.
		return true
	}
	head_branch := os.execute_opt('git ls-remote --symref ${url} HEAD') or {
		vpm_error('failed to find git HEAD for `${url}`.', details: err.msg())
		return false
	}.output.all_after_last('/').all_before(' ').all_before('\t')
	url_ := if url.ends_with('.git') { url.replace('.git', '') } else { url }
	manifest_url := '${url_}/blob/${head_branch}/v.mod'
	vpm_log(@FILE_LINE, @FN, 'manifest_url: ${manifest_url}')
	has_vmod := http.head(manifest_url) or {
		vpm_error('failed to retrieve module data for `${url}`.')
		return false
	}.status_code == 200
	if !has_vmod {
		vpm_error('failed to find `v.mod` for `${url}`.')
		return false
	}
	return true
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

fn get_name_from_url(raw_url string) !string {
	url := urllib.parse(raw_url) or { return error('failed to parse module URL `${raw_url}`.') }
	owner, mut name := url.path.trim_left('/').rsplit_once('/') or {
		return error('failed to retrieve module name for `${url}`.')
	}
	vpm_log(@FILE_LINE, @FN, 'raw_url: ${raw_url}; owner: ${owner}; name: ${name}')
	name = if name.ends_with('.git') { name.replace('.git', '') } else { name }
	return name.replace('-', '_').to_lower()
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
	return vmod.from_file(os.join_path(path, 'v.mod')) or {
		eprintln(term.ecolorize(term.yellow, 'warning: ') +
			'failed to find v.mod file for `${path.all_after_last(os.path_separator)}`.')
		return none
	}
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
