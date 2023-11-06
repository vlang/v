module main

import json
import os
import v.vmod
import sync.pool
import net.http
import net.urllib
import term
import log

struct Mod {
	id           int
	name         string
	url          string
	nr_downloads int
	vcs          string
}

pub struct ModDateInfo {
	name string
mut:
	outdated bool
	exec_err bool
}

struct ModNameInfo {
mut:
	mname             string // The-user.The-mod , *never* The-user.The-mod.git
	mname_normalised  string // the_user.the_mod
	mname_as_path     string // the_user/the_mod
	final_module_path string // ~/.vmodules/the_user/the_mod
}

[params]
struct ErrorOptions {
	details string
	verbose bool // is used to only output the error message if the verbose setting is enabled.
}

fn get_mod_date_info(mut pp pool.PoolProcessor, idx int, wid int) &ModDateInfo {
	mut result := &ModDateInfo{
		name: pp.get_item[string](idx)
	}
	final_module_path := valid_final_path_of_existing_module(result.name) or { return result }
	vcs := vcs_used_in_dir(final_module_path) or { return result }
	is_hg := vcs.cmd == 'hg'
	mut outputs := []string{}
	for step in vcs.args.outdated {
		cmd := '${vcs.cmd} ${vcs.args.path} "${final_module_path}" ${step}'
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

fn get_mod_name_info(mod_name string) ModNameInfo {
	mut info := ModNameInfo{}
	info.mname = if mod_name.ends_with('.git') { mod_name.replace('.git', '') } else { mod_name }
	info.mname_normalised = info.mname.replace('-', '_').to_lower()
	info.mname_as_path = info.mname_normalised.replace('.', os.path_separator)
	info.final_module_path = os.real_path(os.join_path(settings.vmodules_path, info.mname_as_path))
	return info
}

fn get_module_meta_info(name string) !Mod {
	if mod := get_mod_by_url(name) {
		return mod
	}
	if name.len < 2 || (!name[0].is_digit() && !name[0].is_letter()) {
		return error('invalid module name `${name}`.')
	}
	mut errors := []string{}
	for server_url in vpm_server_urls {
		modurl := server_url + '/api/packages/${name}'
		verbose_println('Retrieving module metadata from `${modurl}` ...')
		r := http.get(modurl) or {
			errors << 'Http server did not respond to our request for `${modurl}`.'
			errors << 'Error details: ${err}'
			continue
		}
		if r.status_code == 404 || r.body.trim_space() == '404' {
			errors << 'Skipping module `${name}`, since `${server_url}` reported that `${name}` does not exist.'
			continue
		}
		if r.status_code != 200 {
			errors << 'Skipping module `${name}`, since `${server_url}` responded with ${r.status_code} http status code. Please try again later.'
			continue
		}
		s := r.body
		if s.len > 0 && s[0] != `{` {
			errors << 'Invalid json data'
			errors << s.trim_space().limit(100) + ' ...'
			continue
		}
		mod := json.decode(Mod, s) or {
			errors << 'Skipping module `${name}`, since its information is not in json format.'
			continue
		}
		if '' == mod.url || '' == mod.name {
			errors << 'Skipping module `${name}`, since it is missing name or url information.'
			continue
		}
		return mod
	}
	return error(errors.join_lines())
}

fn get_mod_by_url(name string) !Mod {
	if purl := urllib.parse(name) {
		verbose_println('purl: ${purl}')
		mod := Mod{
			name: purl.path.trim_left('/').trim_right('/').replace('/', '.')
			url: name
		}
		verbose_println(mod.str())
		return mod
	}
	return error('invalid url: ${name}')
}

fn get_outdated() ![]string {
	module_names := get_installed_modules()
	mut outdated := []string{}
	mut pp := pool.new_pool_processor(callback: get_mod_date_info)
	pp.work_on_items(module_names)
	for res in pp.get_results[ModDateInfo]() {
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
		vpm_error(err.msg(),
			verbose: true
		)
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

fn get_working_server_url() string {
	server_urls := if settings.server_urls.len > 0 {
		settings.server_urls
	} else {
		vpm_server_urls
	}
	for url in server_urls {
		verbose_println('Trying server url: ${url}')
		http.head(url) or {
			verbose_println('                   ${url} failed.')
			continue
		}
		return url
	}
	panic('No responding vpm server found. Please check your network connectivity and try again later.')
}

fn ensure_vmodules_dir_exist() {
	if !os.is_dir(settings.vmodules_path) {
		println('Creating `${settings.vmodules_path}` ...')
		os.mkdir(settings.vmodules_path) or {
			vpm_error(err.msg(),
				verbose: true
			)
			exit(1)
		}
	}
}

fn ensure_vcs_is_installed(vcs &VCS) ! {
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

	for server_url in vpm_server_urls {
		modurl := server_url + '/api/packages/${name}/incr_downloads'
		r := http.post(modurl, '') or {
			errors << 'Http server did not respond to our request for `${modurl}`.'
			errors << 'Error details: ${err}'
			continue
		}
		if r.status_code != 200 {
			errors << 'Failed to increment the download count for module `${name}`, since `${server_url}` responded with ${r.status_code} http status code. Please try again later.'
			continue
		}
		return
	}
	return error(errors.join_lines())
}

fn resolve_dependencies(name string, module_path string, module_names []string) {
	vmod_path := os.join_path(module_path, 'v.mod')
	if !os.exists(vmod_path) {
		return
	}
	manifest := vmod.from_file(vmod_path) or {
		vpm_error(err.msg(),
			verbose: true
		)
		return
	}
	// Filter out modules that are both contained in the input query and listed as
	// dependencies in the mod file of the module that is supposed to be installed.
	deps := manifest.dependencies.filter(it !in module_names)
	if deps.len > 0 {
		println('Resolving ${deps.len} dependencies for module `${name}` ...')
		verbose_println('Found dependencies: ${deps}')
		vpm_install(deps)
	}
}

fn url_to_module_name(modulename string) string {
	mut res := if mod := get_mod_by_url(modulename) { mod.name } else { modulename }
	if res.ends_with('.git') {
		res = res.replace('.git', '')
	}
	return res
}

fn vcs_used_in_dir(dir string) ?VCS {
	for vcs in supported_vcs.values() {
		if os.is_dir(os.real_path(os.join_path(dir, vcs.dir))) {
			return vcs
		}
	}
	return none
}

fn valid_final_path_of_existing_module(modulename string) ?string {
	name := if mod := get_mod_by_url(modulename) { mod.name } else { modulename }
	minfo := get_mod_name_info(name)
	if !os.exists(minfo.final_module_path) {
		vpm_error('failed to find a module with name `${minfo.mname_normalised}` at `${minfo.final_module_path}`')
		return none
	}
	if !os.is_dir(minfo.final_module_path) {
		vpm_error('skipping `${minfo.final_module_path}`, since it is not a directory.')
		return none
	}
	vcs_used_in_dir(minfo.final_module_path) or {
		vpm_error('skipping `${minfo.final_module_path}`, since it specifics an unsupported version control system.')
		return none
	}
	return minfo.final_module_path
}

fn verbose_println(s string) {
	if settings.is_verbose {
		println(s)
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
