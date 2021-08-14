// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import regex
import os.cmdline
import net.http
import json
import vhelp
import v.vmod

const (
	default_vpm_server_urls   = ['https://vpm.vlang.io']
	valid_vpm_commands        = ['help', 'search', 'install', 'update', 'upgrade', 'outdated',
		'list', 'remove', 'show']
	excluded_dirs             = ['cache', 'vlib']
	supported_vcs_systems     = ['git', 'hg']
	supported_vcs_folders     = ['.git', '.hg']
	supported_vcs_update_cmds = {
		'git': 'git pull'
		'hg':  'hg pull --update'
	}
	supported_vcs_install_cmds = {
		'git': 'git clone --depth=1'
		'hg':  'hg clone'
	}
	supported_vcs_outdated_steps = {
		'git': ['git fetch', 'git rev-parse @', 'git rev-parse @{u}']
		'hg':  ['hg incoming']
	}
	settings = &VpmSettings{}
)

// settings context:
struct VpmSettings {
mut:
	is_help       bool
	is_verbose    bool
	server_urls   []string
	vmodules_path string
}

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

struct Mod {
	id           int
	url          string
	nr_downloads int
	vcs          string
mut:
	name      string
	installed bool
	updated   bool
}

fn (mod Mod) path() string {
	return real_path_of_module(mod.name)
}

fn real_path_of_module(name string) string {
	mod_name_as_path := name.replace('.', os.path_separator).replace('-', '_').to_lower()
	name_of_vmodules_folder := os.join_path(settings.vmodules_path, mod_name_as_path)
	return os.real_path(name_of_vmodules_folder)
}

fn module_from_url(uri string, vcs string) Mod {
	url := uri.trim_space()
	first_cut_pos := url.last_index('/') or {
		println('Errors while retrieving name for module $url:')
		println(err)
		exit(1)
	}
	mod_name := url.substr(first_cut_pos + 1, url.len)
	second_cut_pos := url.substr(0, first_cut_pos).last_index('/') or {
		println('Errors while retrieving name for module $url:')
		println(err)
		exit(1)
	}
	repo_name := url.substr(second_cut_pos + 1, first_cut_pos)
	return Mod{
		name: '${repo_name}.{$mod_name}'
		url: url
		vcs: vcs
	}
}

fn module_from_manifest(manifest vmod.Manifest) Mod {
	return Mod{
		name: manifest.name
		url: manifest.repo_url
	}
}

fn module_from_file(vpath string) ?Mod {
	manifest := vmod.from_file(vpath) or { panic(err) }
	return module_from_manifest(manifest)
}

fn main() {
	init_settings()
	// This tool is intended to be launched by the v frontend,
	// which provides the path to V inside os.getenv('VEXE')
	// args are: vpm [options] SUBCOMMAND module names
	params := cmdline.only_non_options(os.args[1..])

	verbose_println('cli params: $params')
	if params.len < 1 {
		vpm_help()
		exit(5)
	}
	vpm_command := params[0]
	mut module_names := params[1..]
	ensure_vmodules_dir_exist()
	verbose_println('module names: ')
	println(module_names)
	match vpm_command {
		'help' {
			vpm_help()
		}
		'search' {
			vpm_search(module_names)
		}
		'install' {
			mut modules := parse_modules()

			if modules.len == 0 && os.exists('./v.mod') {
				println('Detected v.mod file inside the project directory. Using it...')
				resolve_dependencies('./v.mod', mut &modules)
			}

			vpm_install(mut &modules)
		}
		'update' {
			mut modules := parse_modules()

			if modules.len == 0 && os.exists('./v.mod') {
				println('Detected v.mod file inside the project directory. Using it...')
				resolve_dependencies('./v.mod', mut &modules)
			}

			vpm_update(mut &modules)
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
			println('Error: you tried to run "v $vpm_command"')
			println('... but the v package management tool vpm only knows about these commands:')
			for validcmd in valid_vpm_commands {
				println('    v $validcmd')
			}
			exit(3)
		}
	}
}

fn parse_modules() []Mod {
	mut modules := []Mod{}
	args := os.args[2..]
	mut re := regex.regex_opt(r'([\w+]+\://)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([/\?\=\&\#\.]?[\w-]+)*/?/gm') or {
		panic(err)
	}

	for git in cmdline.options(args, '-git') {
		modules << module_from_url(git, 'git')
	}

	for hg in cmdline.options(args, '-hg') {
		modules << module_from_url(hg, 'hg')
	}

	mut ignore := true
	for arg in args {
		if ignore {
			ignore = false
			continue
		}
		if arg.starts_with('-') {
			ignore = true
			continue
		}

		// detect urls without option as git
		start, _ := re.match_string(arg)
		if start >= 0 {
			modules << module_from_url(arg, 'git')
			continue
		}

		dep := get_module_meta_info(arg) or {
			println('Errors while retrieving meta data for module $arg:')
			println(err)
			continue
		}
		modules << dep
		continue
	}
	return modules
}

fn vpm_search(keywords []string) {
	search_keys := keywords.map(it.replace('_', '-'))
	if settings.is_help {
		vhelp.show_topic('search')
		exit(0)
	}
	if search_keys.len == 0 {
		println('´v search´ requires *at least one* keyword.')
		exit(2)
	}
	modules := get_all_modules()
	installed_modules := get_installed_modules()
	joined := search_keys.join(', ')
	mut index := 0
	for mod in modules {
		// TODO for some reason .filter results in substr error, so do it manually
		for k in search_keys {
			if !mod.name.contains(k) {
				continue
			}
			if index == 0 {
				println('Search results for "$joined":\n')
			}
			index++
			mut parts := mod.name.split('.')
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
		println('\nUse "v install author_name.module_name" to install the module.')
	}
}

fn vpm_install(mut modules []Mod) {
	mut errors := 0
	for mut mod in modules {
		if mod.installed {
			continue
		}
		mut vcs := mod.vcs
		if vcs == '' {
			vcs = supported_vcs_systems[0]
		}
		if vcs !in supported_vcs_systems {
			errors++
			println('Skipping module "$mod.name", since it uses an unsupported VCS {$vcs} .')
			continue
		}

		mut final_module_path := mod.path()

		if os.exists(final_module_path) {
			mut mods := [mod]
			vpm_update(mut &mods)
			continue
		}
		println('Installing module "$mod.name" from $mod.url to $final_module_path ...')
		vcs_install_cmd := supported_vcs_install_cmds[vcs]
		cmd := '$vcs_install_cmd "$mod.url" "$final_module_path"'
		verbose_println('      command: $cmd')
		cmdres := os.execute(cmd)
		mod.installed = true
		if cmdres.exit_code != 0 {
			errors++
			println('Failed installing module "$mod.name" to "$final_module_path" .')
			verbose_println('Failed command: $cmd')
			verbose_println('Failed command output:\n$cmdres.output')
			continue
		}
		vmod_path := os.join_path(final_module_path, 'v.mod')
		if os.exists(vmod_path) {
			vmod := module_from_file(vmod_path) or {
				println('Error in reading v.mod from "$vmod_path":')
				println(err)
				continue
			}
			mod_path := vmod.path()
			if final_module_path == mod_path {
				continue
			}
			println('Relocating module from "$mod.name" to "$vmod.name" ( $mod_path ) ...')
			if os.exists(mod_path) {
				println('Warning module "$mod_path" already exsits!')
				println('Removing module "$mod_path" ...')
				os.rmdir_all(mod_path) or {
					errors++
					println('Errors while removing "$mod_path" :')
					println(err)
					continue
				}
			}
			os.mv(final_module_path, mod_path) or {
				errors++
				println('Errors while relocating module "$mod.name" :')
				println(err)
				os.rmdir_all(final_module_path) or {
					errors++
					println('Errors while removing "$final_module_path" :')
					println(err)
					continue
				}
				continue
			}
			println('Module "$mod.name" relocated to "$vmod.name" successfully.')
			final_module_path = mod_path
			mod.name = vmod.name
		}
		resolve_dependencies(os.join_path(mod.path(), 'v.mod'), mut &modules)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_update(mut modules []Mod) {
	if settings.is_help {
		vhelp.show_topic('update')
		exit(0)
	}
	if modules.len == 0 {
		return
	}
	mut errors := 0
	for mut mod in modules {
		if mod.updated {
			continue
		}
		final_module_path := mod.path()
		os.chdir(final_module_path)
		println('Updating module "$mod.name"...')
		verbose_println('  work folder: $final_module_path')
		vcs := vcs_used_in_path(final_module_path)
		vcs_cmd := supported_vcs_update_cmds[vcs]
		verbose_println('    command: $vcs_cmd')
		vcs_res := os.execute('$vcs_cmd')
		if vcs_res.exit_code != 0 {
			errors++
			println('Failed updating module "$mod.name".')
			verbose_println('Failed command: $vcs_cmd')
			verbose_println('Failed details:\n$vcs_res.output')
			continue
		} else {
			verbose_println('    $vcs_res.output.trim_space()')
		}
		mod.updated = true
		resolve_dependencies(os.join_path(mod.path(), 'v.mod'), mut &modules)
	}
	if errors > 0 {
		exit(1)
	}
}

fn get_outdated() ?[]Mod {
	modules := get_installed_modules()
	mut outdated := []Mod{}
	for mod in modules {
		final_module_path := mod.path()
		os.chdir(final_module_path)
		vcs := vcs_used_in_path(final_module_path)
		vcs_cmd_steps := supported_vcs_outdated_steps[vcs]
		mut outputs := []string{}
		for step in vcs_cmd_steps {
			res := os.execute(step)
			if res.exit_code < 0 {
				verbose_println('Error command: $step')
				verbose_println('Error details:\n$res.output')
				panic('Error while checking latest commits for "$mod.name".')
			}
			if vcs == 'hg' {
				if res.exit_code == 1 {
					outdated << mod
				}
			} else {
				outputs << res.output
			}
		}
		if vcs == 'git' && outputs[1] != outputs[2] {
			outdated << mod
		}
	}
	return outdated
}

fn vpm_upgrade() {
	mut outdated := get_outdated() or {
		println(err)
		exit(1)
	}
	if outdated.len > 0 {
		vpm_update(mut &outdated)
	} else {
		println('Modules are up to date.')
	}
}

fn vpm_outdated() {
	outdated := get_outdated() or {
		println(err)
		exit(1)
	}
	if outdated.len > 0 {
		println('Outdated modules:')
		for m in outdated {
			println('  $m.name')
		}
	} else {
		println('Modules are up to date.')
	}
}

fn vpm_list() {
	modules := get_installed_modules()
	if modules.len == 0 {
		println('You have no modules installed.')
		exit(0)
	}
	println('Installed modules:')
	for mod in modules {
		println('  $mod.name')
	}
}

fn vpm_remove(module_names []string) {
	if settings.is_help {
		vhelp.show_topic('remove')
		exit(0)
	}
	if module_names.len == 0 {
		println('´v remove´ requires *at least one* module name.')
		exit(2)
	}
	for name in module_names {
		final_module_path := valid_final_path_of_existing_module(name) or { continue }
		println('Removing module "$name"...')
		verbose_println('removing folder $final_module_path')
		os.rmdir_all(final_module_path) or {
			verbose_println('error while removing "$final_module_path": $err.msg')
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
				verbose_println('error while removing "$author_dir": $err.msg')
			}
		}
	}
}

fn valid_final_path_of_existing_module(name string) ?string {
	final_module_path := real_path_of_module(name)
	if !os.exists(final_module_path) {
		println('No module with name "$name" exists at $final_module_path')
		return none
	}
	if !os.is_dir(final_module_path) {
		println('Skipping "$final_module_path", since it is not a folder.')
		return none
	}

	return final_module_path
}

fn ensure_vmodules_dir_exist() {
	if !os.is_dir(settings.vmodules_path) {
		println('Creating $settings.vmodules_path/ ...')
		os.mkdir(settings.vmodules_path) or { panic(err) }
	}
}

fn vpm_help() {
	vhelp.show_topic('vpm')
}

fn vcs_used_in_path(dir string) string {
	for repo_subfolder in supported_vcs_folders {
		checked_folder := os.real_path(os.join_path(dir, repo_subfolder))
		if os.is_dir(checked_folder) {
			return repo_subfolder.replace('.', '')
		}
	}
	return 'git'
}

fn get_installed_modules() []Mod {
	dirs := os.ls(settings.vmodules_path) or { return [] }
	mut modules := []Mod{}
	for dir in dirs {
		adir := os.join_path(settings.vmodules_path, dir)
		if dir in excluded_dirs || !os.is_dir(adir) {
			continue
		}
		mut vmod_path := os.join_path(adir, 'v.mod')
		if os.exists(vmod_path) && os.exists(os.join_path(adir, '.git', 'config')) {
			// an official vlang module with a short module name, like `vsl`, `ui` or `markdown`

			mut mod := module_from_file(vmod_path) or {
				println('Error while reading "$vmod_path":')
				println(err)
				url := 'https://github.com/vlang/$dir'
				modules << Mod{
					name: dir
					url: url
					vcs: 'git'
					installed: true
				}
				continue
			}
			mod.installed = true
			modules << mod
			continue
		}
		author := dir
		mods := os.ls(adir) or { continue }
		for m in mods {
			mut mod := module_from_file(os.join_path(adir, m, 'v.mod')) or {
				println('Error while reading "$vmod_path":')
				println(err)
				name := '${author}.$m'
				url := 'https://github.com/vlang/$author/$m'
				modules << Mod{
					name: name
					url: url
					vcs: 'git'
					installed: true
				}
				continue
			}
			mod.installed = true
			modules << mod
		}
	}
	return modules
}

fn get_all_modules() []Mod {
	url := get_working_server_url()
	r := http.get(url) or { panic(err) }
	if r.status_code != 200 {
		println('Failed to search vpm.vlang.io. Status code: $r.status_code')
		exit(1)
	}
	s := r.text
	mut read_len := 0
	mut names := []string{}
	for read_len < s.len {
		mut start_token := '<a href="/mod'
		end_token := '</a>'
		// get the start index of the module entry
		mut start_index := s.index_after(start_token, read_len)
		if start_index == -1 {
			break
		}
		// get the index of the end of anchor (a) opening tag
		// we use the previous start_index to make sure we are getting a module and not just a random 'a' tag
		start_token = '">'
		start_index = s.index_after(start_token, start_index) + start_token.len
		// get the index of the end of module entry
		end_index := s.index_after(end_token, start_index)
		if end_index == -1 {
			break
		}
		names << s[start_index..end_index]
		read_len = end_index
		if read_len >= s.len {
			break
		}
	}
	mut modules := []Mod{}

	for name in names {
		modules << get_module_meta_info(name) or {
			println('Error in getting "$name" module meta data:')
			println(err)
			continue
		}
	}

	return modules
}

fn is_module_resolved(mod Mod, resolved_modules []Mod) bool {
	for resolved_mod in resolved_modules {
		// because name may be based of vcs url is not good comparison
		if resolved_mod.url == mod.url {
			return true
		}
	}

	return false
}

fn resolve_dependencies(path string, mut modules []Mod) {
	manifest := vmod.from_file(path) or { return }

	mut deps := []string{}
	for dep in manifest.dependencies {
		mod := get_module_meta_info(dep) or {
			println('Errors while retrieving meta data for module $dep:')
			println(err)
			continue
		}
		if !is_module_resolved(mod, modules) {
			modules << mod
			deps << mod.name
		}
	}

	for url in manifest.git {
		mod := module_from_url(url, 'git')
		if !is_module_resolved(mod, modules) {
			modules << mod
			deps << mod.name
		}
	}

	for url in manifest.hg {
		mod := module_from_url(url, 'hg')
		if !is_module_resolved(mod, modules) {
			modules << mod
			deps << mod.name
		}
	}

	if deps.len > 0 {
		println('Resolving $deps.len dependencies for module "$manifest.name"...')
		verbose_println('Found dependencies: $deps')
		vpm_install(mut &modules)
	}
}

fn get_working_server_url() string {
	server_urls := if settings.server_urls.len > 0 {
		settings.server_urls
	} else {
		default_vpm_server_urls
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

fn verbose_println(s string) {
	if settings.is_verbose {
		println(s)
	}
}

fn get_module_meta_info(name string) ?Mod {
	mut errors := []string{}
	for server_url in default_vpm_server_urls {
		modurl := server_url + '/jsmod/$name'
		verbose_println('Retrieving module metadata from: $modurl ...')
		r := http.get(modurl) or {
			errors << 'Http server did not respond to our request for ${modurl}.'
			errors << 'Error details: $err'
			continue
		}
		if r.status_code == 404 || r.text.trim_space() == '404' {
			errors << 'Skipping module "$name", since $server_url reported that "$name" does not exist.'
			continue
		}
		if r.status_code != 200 {
			errors << 'Skipping module "$name", since $server_url responded with $r.status_code http status code. Please try again later.'
			continue
		}
		s := r.text
		if s.len > 0 && s[0] != `{` {
			errors << 'Invalid json data'
			errors << s.trim_space().limit(100) + '...'
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
	mut installed_modules_names := []string{}
	for installed in installed_modules {
		installed_modules_names << installed.name
	}
	for module_name in module_names {
		if module_name !in installed_modules_names {
			module_meta_info := get_module_meta_info(module_name) or { continue }
			println('Name: $module_meta_info.name')
			println('Homepage: $module_meta_info.url')
			println('Downloads: $module_meta_info.nr_downloads')
			println('Installed: False')
			println('--------')
			continue
		}
		path := real_path_of_module(module_name)
		mod := vmod.from_file(path) or { continue }
		println('Name: $mod.name')
		println('Version: $mod.version')
		println('Description: $mod.description')
		println('Homepage: $mod.repo_url')
		println('Author: $mod.author')
		println('License: $mod.license')
		println('Location: $path')
		println('Requires: ${mod.dependencies.join(', ')}')
		println('--------')
	}
}
