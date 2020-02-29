module main

import (
	net.http
	os
	os.cmdline
	json
	filepath
)

const (
	default_vpm_server_urls = ['https://vpm.best', 'https://vpm.vlang.io']
	valid_vpm_commands = ['help', 'search', 'install', 'update', 'remove']
	excluded_dirs = ['cache', 'vlib']
	supported_vcs_systems = ['git', 'hg']
	supported_vcs_folders = ['.git', '.hg']
	supported_vcs_update_cmds = {
		'git': 'git pull --depth=1'
		'hg': 'hg pull --update'
	}
	supported_vcs_install_cmds = {
		'git': 'git clone --depth=1'
		'hg': 'hg clone'
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

fn main() {
	init_settings()
	// This tool is intended to be launched by the v frontend,
	// which provides the path to V inside os.getenv('VEXE')
	args := os.args // args are: vpm [options] SUBCOMMAND module names
	params := cmdline.only_non_options(args[1..])
	verbose_println('cli params: $params')
	if params.len < 1 {
		vpm_help([])
		exit(5)
	}
	vpm_command := params[0]
	module_names := params[1..]
	ensure_vmodules_dir_exist()
	// println('module names: ') println(module_names)
	match vpm_command {
		'help' {
			vpm_help(module_names)
		}
		'search' {
			vpm_search(module_names)
		}
		'install' {
			vpm_install(module_names)
		}
		'update' {
			vpm_update(module_names)
		}
		'remove' {
			vpm_remove(module_names)
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

fn vpm_search(keywords []string) {
	if settings.is_help {
		println('Usage:')
		println('  v search keyword1 [keyword2] [...]')
		println('  ^^^^^^^^^^^^^^^^^ will search https://vpm.vlang.io/ for matching modules,')
		println('                    and will show details about them')
		show_vpm_options()
		exit(0)
	}
	if keywords.len == 0 {
		println('  v search requires *at least one* keyword')
		exit(2)
	}
	modules := get_all_modules()
	joined := keywords.join(', ')
	mut index := 0
	for mod in modules {
		// TODO for some reason .filter results in substr error, so do it manually
		for k in keywords {
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
				parts[0] = ''
			}
			println('${index}. ${parts[1]} by ${parts[0]} [$mod]')
			break
		}
	}
	println('\nUse "v install author.module_name" to install the module')
	if index == 0 {
		println('No module(s) found for "$joined"')
	}
}

fn vpm_install(module_names []string) {
	if settings.is_help {
		println('Usage:')
		println('  v install module [module] [module] [...]')
		println('  ^^^^^^^^^^^^^ will install the modules you specified')
		show_vpm_options()
		exit(0)
	}
	if module_names.len == 0 {
		println('  v install requires *at least one* module name')
		exit(2)
	}
	mut errors := 0
	url := get_working_server_url()
	for n in module_names {
		name := n.trim_space()
		modurl := url + '/jsmod/$name'
		r := http.get(modurl) or {
			println('Http server did not respond to our request for ${modurl}.')
			println('Error details: $err')
			errors++
			continue
		}
		if r.status_code == 404 {
			println('Skipping module "$name", since $url reported that "$name" does not exist.')
			errors++
			continue
		}
		if r.status_code != 200 {
			println('Skipping module "$name", since $url responded with $r.status_code http status code. Please try again later.')
			errors++
			continue
		}
		s := r.text
		mod := json.decode(Mod,s) or {
			errors++
			println('Skipping module "$name", since its information is not in json format.')
			continue
		}
		if ('' == mod.url || '' == mod.name) {
			errors++
			// a possible 404 error, which means a missing module?
			println('Skipping module "$name", since it is missing name or url information.')
			continue
		}
		mut vcs := mod.vcs
		if vcs == '' {
			vcs = supported_vcs_systems[0]
		}
		if !vcs in supported_vcs_systems {
			errors++
			println('Skipping module "$name", since it uses an unsupported VCS {$vcs} .')
			continue
		}
		final_module_path := os.realpath(filepath.join(settings.vmodules_path,mod.name.replace('.', filepath.separator)))
		if os.exists(final_module_path) {
			vpm_update([name])
			continue
		}
		println('Installing module "$name" from $mod.url to $final_module_path ...')
		vcs_install_cmd := supported_vcs_install_cmds[vcs]
		cmd := '${vcs_install_cmd} "${mod.url}" "${final_module_path}"'
		verbose_println('      command: $cmd')
		cmdres := os.exec(cmd) or {
			errors++
			println('Could not install module "$name" to "$final_module_path" .')
			verbose_println('Error command: $cmd')
			verbose_println('Error details: $err')
			continue
		}
		if cmdres.exit_code != 0 {
			errors++
			println('Failed installing module "$name" to "$final_module_path" .')
			verbose_println('Failed command: ${cmd}')
			verbose_println('Failed command output:\n${cmdres.output}')
			continue
		}
		resolve_dependencies(name, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_update(m []string) {
	mut module_names := m
	if settings.is_help {
		println('Usage: ')
		println(' a) v update module [module] [module] [...]')
		println('    ^^^^^^^^^^^^ will update the listed modules to their latest versions')
		println(' b) v update')
		println('    ^^^^^^^^^^^^ will update ALL installed modules to their latest versions')
		show_vpm_options()
		exit(0)
	}
	if module_names.len == 0 {
		module_names = get_installed_modules()
	}
	mut errors := 0
	for name in module_names {
		final_module_path := valid_final_path_of_existing_module(name) or {
			continue
		}
		os.chdir(final_module_path)
		println('Updating module "$name"...')
		verbose_println('  work folder: $final_module_path')
		vcs := vcs_used_in_dir(final_module_path) or {
			continue
		}
		vcs_cmd := supported_vcs_update_cmds[vcs[0]]
		verbose_println('      command: $vcs_cmd')
		vcs_res := os.exec('${vcs_cmd}') or {
			errors++
			println('Could not update module "$name".')
			verbose_println('Error command: ${vcs_cmd}')
			verbose_println('Error details:\n$err')
			continue
		}
		if vcs_res.exit_code != 0 {
			errors++
			println('Failed updating module "${name}".')
			verbose_println('Failed command: ${vcs_cmd}')
			verbose_println('Failed details:\n${vcs_res.output}')
			continue
		}
		resolve_dependencies(name, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_remove(module_names []string) {
	if settings.is_help {
		println('Usage: ')
		println(' a) v remove module [module] [module] [...]')
		println('    ^^^^^^^^^^^^ will remove the listed modules')
		println(' b) v remove')
		println('    ^^^^^^^^^^^^ will remove ALL installed modules')
		show_vpm_options()
		exit(0)
	}
	if module_names.len == 0 {
		println('  v update requires *at least one* module name')
		exit(2)
	}
	for name in module_names {
		final_module_path := valid_final_path_of_existing_module(name) or {
			continue
		}
		println('Removing module "$name"...')
		verbose_println('removing folder $final_module_path')
		os.rmdir_all(final_module_path)
		// delete author directory if it is empty
		author := name.split('.')[0]
		author_dir := os.realpath(filepath.join(settings.vmodules_path,author))
		if os.is_dir_empty(author_dir) {
			verbose_println('removing author folder $author_dir')
			os.rmdir(author_dir)
		}
	}
}

fn valid_final_path_of_existing_module(name string) ?string {
	name_of_vmodules_folder := filepath.join(settings.vmodules_path,name.replace('.', filepath.separator))
	final_module_path := os.realpath(name_of_vmodules_folder)
	if !os.exists(final_module_path) {
		println('No module with name "$name" exists at $name_of_vmodules_folder')
		return none
	}
	if !os.is_dir(final_module_path) {
		println('Skipping "$name_of_vmodules_folder", since it is not a folder.')
		return none
	}
	vcs_used_in_dir(final_module_path) or {
		println('Skipping "$name_of_vmodules_folder", since it does not use a supported vcs.')
		return none
	}
	return final_module_path
}

fn ensure_vmodules_dir_exist() {
	if !os.is_dir(settings.vmodules_path) {
		println('Creating $settings.vmodules_path/ ...')
		os.mkdir(settings.vmodules_path) or {
			panic(err)
		}
	}
}

fn vpm_help(module_names []string) {
	println('Usage:')
	println('  a) v install module [module] [module] [...]')
	println('  b) v update [module] [...]')
	println('  c) v remove [module] [...]')
	println('  d) v search keyword1 [keyword2] [...]')
	println('')
	println('  You can also pass -h or --help after each vpm command from the above, to see more details about it.')
}

fn vcs_used_in_dir(dir string) ?[]string {
	mut vcs := []string
	for repo_subfolder in supported_vcs_folders {
		checked_folder := os.realpath(filepath.join(dir,repo_subfolder))
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
	dirs := os.ls(settings.vmodules_path) or {
		return []
	}
	mut modules := []string
	for dir in dirs {
		adir := filepath.join(settings.vmodules_path,dir)
		if dir in excluded_dirs || !os.is_dir(adir) {
			continue
		}
		author := dir
		mods := os.ls(adir) or {
			continue
		}
		for m in mods {
			vcs_used_in_dir(filepath.join(adir,m)) or {
				continue
			}
			modules << '${author}.$m'
		}
	}
	return modules
}

fn get_all_modules() []string {
	url := get_working_server_url()
	r := http.get(url) or {
		panic(err)
	}
	if r.status_code != 200 {
		println('Failed to search vpm.best. Status code: $r.status_code')
		exit(1)
	}
	s := r.text
	mut read_len := 0
	mut modules := []string
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
		modules << s[start_index..end_index]
		read_len = end_index
		if read_len >= s.len {
			break
		}
	}
	return modules
}

fn resolve_dependencies(name, module_path string, module_names []string) {
	vmod_path := filepath.join(module_path,'v.mod')
	if !os.exists(vmod_path) {
		return
	}
	data := os.read_file(vmod_path) or {
		return
	}
	vmod := parse_vmod(data)
	mut deps := []string
	// filter out dependencies that were already specified by the user
	for d in vmod.deps {
		if !(d in module_names) {
			deps << d
		}
	}
	if deps.len > 0 {
		println('Resolving ${deps.len} dependencies for module "$name"...')
		verbose_println('Found dependencies: $deps')
		vpm_install(deps)
	}
}

fn parse_vmod(data string) Vmod {
	keys := ['name', 'version', 'deps']
	mut m := {
		'name': '',
		'version': '',
		'deps': ''
	}
	for key in keys {
		mut key_index := data.index('$key:') or {
			continue
		}
		key_index += key.len + 1
		m[key] = data[key_index..data.index_after('\n', key_index)].trim_space().replace("'", '').replace('[', '').replace(']', '')
	}
	mut vmod := Vmod{}
	vmod.name = m['name']
	vmod.version = m['version']
	if m['deps'].len > 0 {
		vmod.deps = m['deps'].split(',')
	}
	return vmod
}

fn get_working_server_url() string {
	server_urls := if settings.server_urls.len > 0 { settings.server_urls } else { default_vpm_server_urls }
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
	unsafe{
		s = settings
	}
	s.is_help = '-h' in os.args || '--help' in os.args || 'help' in os.args
	s.is_verbose = '-verbose' in os.args || '--verbose' in os.args
	s.server_urls = cmdline.options(os.args, '-server-url')
	s.vmodules_path = os.home_dir() + '.vmodules'
}

fn show_vpm_options() {
	println('Options:')
	println('  -help        - Show usage info')
	println('  -verbose     - Print more details about the performed operation')
	println('  -server-url  - When doing network operations, use this vpm server. Can be given multiple times.')
}

fn verbose_println(s string) {
	if settings.is_verbose {
		println(s)
	}
}
