// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import rand
import os.cmdline
import net.http
import net.urllib
import json
import vhelp
import v.vmod

const (
	default_vpm_server_urls   = ['https://vpm.vlang.io', 'https://vpm.url4e.com']
	vpm_server_urls           = rand.shuffle_clone(default_vpm_server_urls) or { [] } // ensure that all queries are distributed fairly
	valid_vpm_commands        = ['help', 'search', 'install', 'update', 'upgrade', 'outdated',
		'list', 'remove', 'show']
	excluded_dirs             = ['cache', 'vlib']
	supported_vcs_systems     = ['git', 'hg']
	supported_vcs_folders     = ['.git', '.hg']
	supported_vcs_update_cmds = {
		'git': 'git pull --recurse-submodules' // pulling with `--depth=1` leads to conflicts, when the upstream is more than 1 commit newer
		'hg':  'hg pull --update'
	}
	supported_vcs_install_cmds = {
		'git': 'git clone --depth=1 --recursive --shallow-submodules'
		'hg':  'hg clone'
	}
	supported_vcs_outdated_steps = {
		'git': ['git fetch', 'git rev-parse @', 'git rev-parse @\{u}']
		'hg':  ['hg incoming']
	}
	supported_vcs_version_cmds = {
		'git': 'git version'
		'hg':  'hg version'
	}
)

struct Mod {
	id           int
	name         string
	url          string
	nr_downloads int
	vcs          string
}

struct Vmod {
mut:
	name    string
	version string
	deps    []string
}

enum Source {
	git
	hg
	vpm
}

fn main() {
	init_settings()
	// This tool is intended to be launched by the v frontend,
	// which provides the path to V inside os.getenv('VEXE')
	// args are: vpm [options] SUBCOMMAND module names
	params := cmdline.only_non_options(os.args[1..])
	options := cmdline.only_options(os.args[1..])
	verbose_println('cli params: $params')
	if params.len < 1 {
		vpm_help()
		exit(5)
	}
	vpm_command := params[0]
	mut module_names := params[1..]
	ensure_vmodules_dir_exist()
	// println('module names: ') println(module_names)
	match vpm_command {
		'help' {
			vpm_help()
		}
		'search' {
			vpm_search(module_names)
		}
		'install' {
			if module_names.len == 0 && os.exists('./v.mod') {
				println('Detected v.mod file inside the project directory. Using it...')
				manifest := vmod.from_file('./v.mod') or { panic(err) }
				module_names = manifest.dependencies.clone()
			}
			mut source := Source.vpm
			if module_names.all(it.starts_with('https://')) {
				source = Source.git
			}
			if '--once' in options {
				module_names = vpm_once_filter(module_names)
				if module_names.len == 0 {
					return
				}
			}
			if '--git' in options {
				source = Source.git
			}
			if '--hg' in options {
				source = Source.hg
			}

			vpm_install(module_names, source)
		}
		'update' {
			vpm_update(module_names)
		}
		'upgrade' {
			vpm_upgrade()
		}
		'outdated' {
			vpm_outdated()
		}
		'list' {
			vpm_list()
		}
		'remove' {
			vpm_remove(module_names)
		}
		'show' {
			vpm_show(module_names)
		}
		else {
			eprintln('Error: you tried to run "v $vpm_command"')
			eprintln('... but the v package management tool vpm only knows about these commands:')
			for validcmd in valid_vpm_commands {
				eprintln('    v $validcmd')
			}
			exit(3)
		}
	}
}

fn vpm_search(keywords []string) {
	search_keys := keywords.map(it.replace('_', '-'))
	if settings.is_help {
		vhelp.show_topic('search')
		exit(0)
	}
	if search_keys.len == 0 {
		eprintln('´v search´ requires *at least one* keyword.')
		exit(2)
	}
	modules := get_all_modules()
	installed_modules := get_installed_modules()
	joined := search_keys.join(', ')
	mut index := 0
	for mod in modules {
		for k in search_keys {
			if !mod.contains(k) {
				continue
			}
			if index == 0 {
				println('Search results for "$joined":\n')
			}
			index++
			mut parts := mod.split('.')
			// in case the author isn't present
			if parts.len == 1 {
				parts << parts[0]
				parts[0] = ' '
			} else {
				parts[0] = ' by ${parts[0]} '
			}
			installed := if mod in installed_modules { ' (installed)' } else { '' }
			println('${index}. ${parts[1]}${parts[0]}[$mod]$installed')
			break
		}
	}
	if index == 0 {
		vexe := os.getenv('VEXE')
		vroot := os.real_path(os.dir(vexe))
		mut messages := ['No module(s) found for `$joined` .']
		for vlibmod in search_keys {
			if os.is_dir(os.join_path(vroot, 'vlib', vlibmod)) {
				messages << 'There is already an existing "$vlibmod" module in vlib, so you can just `import $vlibmod` .'
			}
		}
		for m in messages {
			println(m)
		}
	} else {
		eprintln('\nUse "v install author_name.module_name" to install the module.')
	}
}

fn vpm_install_from_vpm(module_names []string) {
	mut errors := 0
	for n in module_names {
		name := n.trim_space().replace('_', '-')
		mod := get_module_meta_info(name) or {
			errors++
			eprintln('Errors while retrieving meta data for module $name:')
			eprintln(err)
			continue
		}
		mut vcs := mod.vcs
		if vcs == '' {
			vcs = supported_vcs_systems[0]
		}
		if vcs !in supported_vcs_systems {
			errors++
			eprintln('Skipping module "$name", since it uses an unsupported VCS {$vcs} .')
			continue
		}
		if !ensure_vcs_is_installed(vcs) {
			errors++
			eprintln('VPM needs `$vcs` to be installed.')
			continue
		}
		//
		minfo := mod_name_info(mod.name)
		if os.exists(minfo.final_module_path) {
			vpm_update([name])
			continue
		}
		println('Installing module "$name" from "$mod.url" to "$minfo.final_module_path" ...')
		vcs_install_cmd := supported_vcs_install_cmds[vcs]
		cmd := '$vcs_install_cmd "$mod.url" "$minfo.final_module_path"'
		verbose_println('      command: $cmd')
		cmdres := os.execute(cmd)
		if cmdres.exit_code != 0 {
			errors++
			eprintln('Failed installing module "$name" to "$minfo.final_module_path" .')
			print_failed_cmd(cmd, cmdres)
			continue
		}
		resolve_dependencies(name, minfo.final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn print_failed_cmd(cmd string, cmdres os.Result) {
	verbose_println('Failed command: $cmd')
	verbose_println('Failed command output:\n$cmdres.output')
}

fn ensure_vcs_is_installed(vcs string) bool {
	mut res := true
	cmd := supported_vcs_version_cmds[vcs]
	cmdres := os.execute(cmd)
	if cmdres.exit_code != 0 {
		print_failed_cmd(cmd, cmdres)
		res = false
	}
	return res
}

fn vpm_install_from_vcs(module_names []string, vcs_key string) {
	mut errors := 0
	for n in module_names {
		url := n.trim_space()

		first_cut_pos := url.last_index('/') or {
			errors++
			eprintln('Errors while retrieving name for module "$url" :')
			eprintln(err)
			continue
		}

		mod_name := url.substr(first_cut_pos + 1, url.len)

		second_cut_pos := url.substr(0, first_cut_pos).last_index('/') or {
			errors++
			eprintln('Errors while retrieving name for module "$url" :')
			eprintln(err)
			continue
		}

		repo_name := url.substr(second_cut_pos + 1, first_cut_pos)
		mut name := os.join_path(repo_name, mod_name)
		mod_name_as_path := name.replace('-', '_').to_lower()
		mut final_module_path := os.real_path(os.join_path(settings.vmodules_path, mod_name_as_path))
		if os.exists(final_module_path) {
			vpm_update([name.replace('-', '_')])
			continue
		}
		if !ensure_vcs_is_installed(vcs_key) {
			errors++
			eprintln('VPM needs `$vcs_key` to be installed.')
			continue
		}
		println('Installing module "$name" from "$url" to "$final_module_path" ...')
		vcs_install_cmd := supported_vcs_install_cmds[vcs_key]
		cmd := '$vcs_install_cmd "$url" "$final_module_path"'
		verbose_println('      command: $cmd')
		cmdres := os.execute(cmd)
		if cmdres.exit_code != 0 {
			errors++
			eprintln('Failed installing module "$name" to "$final_module_path" .')
			print_failed_cmd(cmd, cmdres)
			continue
		}
		vmod_path := os.join_path(final_module_path, 'v.mod')
		if os.exists(vmod_path) {
			data := os.read_file(vmod_path) or { return }
			vmod := parse_vmod(data) or {
				eprintln(err)
				return
			}
			minfo := mod_name_info(vmod.name)
			println('Relocating module from "$name" to "$vmod.name" ( "$minfo.final_module_path" ) ...')
			if os.exists(minfo.final_module_path) {
				eprintln('Warning module "$minfo.final_module_path" already exsits!')
				eprintln('Removing module "$minfo.final_module_path" ...')
				os.rmdir_all(minfo.final_module_path) or {
					errors++
					println('Errors while removing "$minfo.final_module_path" :')
					println(err)
					continue
				}
			}
			os.mv(final_module_path, minfo.final_module_path) or {
				errors++
				eprintln('Errors while relocating module "$name" :')
				eprintln(err)
				os.rmdir_all(final_module_path) or {
					errors++
					eprintln('Errors while removing "$final_module_path" :')
					eprintln(err)
					continue
				}
				continue
			}
			println('Module "$name" relocated to "$vmod.name" successfully.')
			final_module_path = minfo.final_module_path
			name = vmod.name
		}
		resolve_dependencies(name, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_once_filter(module_names []string) []string {
	installed_modules := get_installed_modules()
	mut toinstall := []string{}
	for mn in module_names {
		if mn !in installed_modules {
			toinstall << mn
		}
	}
	return toinstall
}

fn vpm_install(module_names []string, source Source) {
	if settings.is_help {
		vhelp.show_topic('install')
		exit(0)
	}
	if module_names.len == 0 {
		eprintln('´v install´ requires *at least one* module name.')
		exit(2)
	}
	match source {
		.vpm {
			vpm_install_from_vpm(module_names)
		}
		.git {
			vpm_install_from_vcs(module_names, 'git')
		}
		.hg {
			vpm_install_from_vcs(module_names, 'hg')
		}
	}
}

fn vpm_update(m []string) {
	mut module_names := m.clone()
	if settings.is_help {
		vhelp.show_topic('update')
		exit(0)
	}
	if module_names.len == 0 {
		module_names = get_installed_modules()
	}
	mut errors := 0
	for modulename in module_names {
		zname := url_to_module_name(modulename)
		final_module_path := valid_final_path_of_existing_module(modulename) or { continue }
		os.chdir(final_module_path) or {}
		println('Updating module "$zname" in "$final_module_path" ...')
		vcs := vcs_used_in_dir(final_module_path) or { continue }
		if !ensure_vcs_is_installed(vcs[0]) {
			errors++
			println('VPM needs `$vcs` to be installed.')
			continue
		}
		vcs_cmd := supported_vcs_update_cmds[vcs[0]]
		verbose_println('    command: $vcs_cmd')
		vcs_res := os.execute('$vcs_cmd')
		if vcs_res.exit_code != 0 {
			errors++
			println('Failed updating module "$zname" in "$final_module_path" .')
			print_failed_cmd(vcs_cmd, vcs_res)
			continue
		} else {
			verbose_println('    $vcs_res.output.trim_space()')
		}
		resolve_dependencies(modulename, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn get_outdated() ?[]string {
	module_names := get_installed_modules()
	mut outdated := []string{}
	for name in module_names {
		final_module_path := valid_final_path_of_existing_module(name) or { continue }
		os.chdir(final_module_path) or {}
		vcs := vcs_used_in_dir(final_module_path) or { continue }
		vcs_cmd_steps := supported_vcs_outdated_steps[vcs[0]]
		mut outputs := []string{}
		for step in vcs_cmd_steps {
			res := os.execute(step)
			if res.exit_code < 0 {
				verbose_println('Error command: $step')
				verbose_println('Error details:\n$res.output')
				return error('Error while checking latest commits for "$name" .')
			}
			if vcs[0] == 'hg' {
				if res.exit_code == 1 {
					outdated << name
				}
			} else {
				outputs << res.output
			}
		}
		if vcs[0] == 'git' && outputs[1] != outputs[2] {
			outdated << name
		}
	}
	return outdated
}

fn vpm_upgrade() {
	outdated := get_outdated() or { exit(1) }
	if outdated.len > 0 {
		vpm_update(outdated)
	} else {
		println('Modules are up to date.')
	}
}

fn vpm_outdated() {
	outdated := get_outdated() or { exit(1) }
	if outdated.len > 0 {
		eprintln('Outdated modules:')
		for m in outdated {
			eprintln('  $m')
		}
	} else {
		println('Modules are up to date.')
	}
}

fn vpm_list() {
	module_names := get_installed_modules()
	if module_names.len == 0 {
		eprintln('You have no modules installed.')
		exit(0)
	}
	for mod in module_names {
		println(mod)
	}
}

fn vpm_remove(module_names []string) {
	if settings.is_help {
		vhelp.show_topic('remove')
		exit(0)
	}
	if module_names.len == 0 {
		eprintln('´v remove´ requires *at least one* module name.')
		exit(2)
	}
	for name in module_names {
		final_module_path := valid_final_path_of_existing_module(name) or { continue }
		eprintln('Removing module "$name" ...')
		verbose_println('removing folder $final_module_path')
		os.rmdir_all(final_module_path) or {
			verbose_println('error while removing "$final_module_path": $err.msg()')
		}
		// delete author directory if it is empty
		author := name.split('.')[0]
		author_dir := os.real_path(os.join_path(settings.vmodules_path, author))
		if !os.exists(author_dir) {
			continue
		}
		if os.is_dir_empty(author_dir) {
			verbose_println('removing author folder $author_dir')
			os.rmdir(author_dir) or {
				verbose_println('error while removing "$author_dir": $err.msg()')
			}
		}
	}
}

fn valid_final_path_of_existing_module(modulename string) ?string {
	name := if mod := get_mod_by_url(modulename) { mod.name } else { modulename }
	minfo := mod_name_info(name)
	if !os.exists(minfo.final_module_path) {
		eprintln('No module with name "$minfo.mname_normalised" exists at $minfo.final_module_path')
		return none
	}
	if !os.is_dir(minfo.final_module_path) {
		eprintln('Skipping "$minfo.final_module_path", since it is not a folder.')
		return none
	}
	vcs_used_in_dir(minfo.final_module_path) or {
		eprintln('Skipping "$minfo.final_module_path", since it does not use a supported vcs.')
		return none
	}
	return minfo.final_module_path
}

fn ensure_vmodules_dir_exist() {
	if !os.is_dir(settings.vmodules_path) {
		println('Creating "$settings.vmodules_path/" ...')
		os.mkdir(settings.vmodules_path) or { panic(err) }
	}
}

fn vpm_help() {
	vhelp.show_topic('vpm')
}

fn vcs_used_in_dir(dir string) ?[]string {
	mut vcs := []string{}
	for repo_subfolder in supported_vcs_folders {
		checked_folder := os.real_path(os.join_path(dir, repo_subfolder))
		if os.is_dir(checked_folder) {
			vcs << repo_subfolder.replace('.', '')
		}
	}
	if vcs.len == 0 {
		return none
	}
	return vcs
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
			modules << '${author}.$m'
		}
	}
	return modules
}

struct ModNameInfo {
mut:
	mname             string // The-user.The-mod , *never* The-user.The-mod.git
	mname_normalised  string // the_user.the_mod
	mname_as_path     string // the_user/the_mod
	final_module_path string // ~/.vmodules/the_user/the_mod
}

fn mod_name_info(mod_name string) ModNameInfo {
	mut info := ModNameInfo{}
	info.mname = if mod_name.ends_with('.git') { mod_name.replace('.git', '') } else { mod_name }
	info.mname_normalised = info.mname.replace('-', '_').to_lower()
	info.mname_as_path = info.mname_normalised.replace('.', os.path_separator)
	info.final_module_path = os.real_path(os.join_path(settings.vmodules_path, info.mname_as_path))
	return info
}

fn url_to_module_name(modulename string) string {
	mut res := if mod := get_mod_by_url(modulename) { mod.name } else { modulename }
	if res.ends_with('.git') {
		res = res.replace('.git', '')
	}
	return res
}

fn get_all_modules() []string {
	url := get_working_server_url()
	r := http.get(url) or { panic(err) }
	if r.status_code != 200 {
		eprintln('Failed to search vpm.vlang.io. Status code: $r.status_code')
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

fn resolve_dependencies(name string, module_path string, module_names []string) {
	vmod_path := os.join_path(module_path, 'v.mod')
	if !os.exists(vmod_path) {
		return
	}
	data := os.read_file(vmod_path) or { return }
	vmod := parse_vmod(data) or {
		eprintln(err)
		return
	}
	mut deps := []string{}
	// filter out dependencies that were already specified by the user
	for d in vmod.deps {
		if d !in module_names {
			deps << d
		}
	}
	if deps.len > 0 {
		println('Resolving $deps.len dependencies for module "$name" ...')
		verbose_println('Found dependencies: $deps')
		vpm_install(deps, Source.vpm)
	}
}

fn parse_vmod(data string) !Vmod {
	manifest := vmod.decode(data) or { return error('Parsing v.mod file failed, $err') }
	mut vmod := Vmod{}
	vmod.name = manifest.name
	vmod.version = manifest.version
	vmod.deps = manifest.dependencies
	return vmod
}

fn get_working_server_url() string {
	server_urls := if settings.server_urls.len > 0 {
		settings.server_urls
	} else {
		vpm_server_urls
	}
	for url in server_urls {
		verbose_println('Trying server url: $url')
		http.head(url) or {
			verbose_println('                   $url failed.')
			continue
		}
		return url
	}
	panic('No responding vpm server found. Please check your network connectivity and try again later.')
}

// settings context:
struct VpmSettings {
mut:
	is_help       bool
	is_verbose    bool
	server_urls   []string
	vmodules_path string
}

const (
	settings = &VpmSettings{}
)

fn init_settings() {
	mut s := &VpmSettings(0)
	unsafe {
		s = settings
	}
	s.is_help = '-h' in os.args || '--help' in os.args || 'help' in os.args
	s.is_verbose = '-v' in os.args
	s.server_urls = cmdline.options(os.args, '-server-url')
	s.vmodules_path = os.vmodules_dir()
}

fn verbose_println(s string) {
	if settings.is_verbose {
		println(s)
	}
}

fn get_mod_by_url(name string) ?Mod {
	if purl := urllib.parse(name) {
		verbose_println('purl: $purl')
		mod := Mod{
			name: purl.path.trim_left('/').trim_right('/').replace('/', '.')
			url: name
		}
		verbose_println(mod.str())
		return mod
	}
	return error('invalid url: $name')
}

fn get_module_meta_info(name string) ?Mod {
	if mod := get_mod_by_url(name) {
		return mod
	}
	mut errors := []string{}

	for server_url in vpm_server_urls {
		modurl := server_url + '/jsmod/$name'
		verbose_println('Retrieving module metadata from: "$modurl" ...')
		r := http.get(modurl) or {
			errors << 'Http server did not respond to our request for "$modurl" .'
			errors << 'Error details: $err'
			continue
		}
		if r.status_code == 404 || r.body.trim_space() == '404' {
			errors << 'Skipping module "$name", since "$server_url" reported that "$name" does not exist.'
			continue
		}
		if r.status_code != 200 {
			errors << 'Skipping module "$name", since "$server_url" responded with $r.status_code http status code. Please try again later.'
			continue
		}
		s := r.body
		if s.len > 0 && s[0] != `{` {
			errors << 'Invalid json data'
			errors << s.trim_space().limit(100) + ' ...'
			continue
		}
		mod := json.decode(Mod, s) or {
			errors << 'Skipping module "$name", since its information is not in json format.'
			continue
		}
		if '' == mod.url || '' == mod.name {
			errors << 'Skipping module "$name", since it is missing name or url information.'
			continue
		}
		return mod
	}
	return error(errors.join_lines())
}

fn vpm_show(module_names []string) {
	installed_modules := get_installed_modules()
	for module_name in module_names {
		if module_name !in installed_modules {
			module_meta_info := get_module_meta_info(module_name) or { continue }
			print('
Name: $module_meta_info.name
Homepage: $module_meta_info.url
Downloads: $module_meta_info.nr_downloads
Installed: False
--------
')
			continue
		}
		path := os.join_path(os.vmodules_dir(), module_name.replace('.', os.path_separator))
		mod := vmod.from_file(os.join_path(path, 'v.mod')) or { continue }
		print('Name: $mod.name
Version: $mod.version
Description: $mod.description
Homepage: $mod.repo_url
Author: $mod.author
License: $mod.license
Location: $path
Requires: ${mod.dependencies.join(', ')}
--------
')
	}
}
