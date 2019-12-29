module main

import (
	http
	os
	json
	filepath
)

const (
// url = 'http://localhost:8089'
	url = 'https://vpm.best'
	valid_vpm_commands = ['help', 'search', 'install', 'update', 'remove']
)

struct Mod {
	id           int
	name         string
	url          string
	nr_downloads int
}

struct Vmod {
mut:
	name    string
	version string
	deps    []string
}

fn main() {
	ensure_vmodules_dir_exist()
	change_to_vmodules_dir()
	// This tool is intended to be launched by the v frontend,
	// which provides the path to V inside os.getenv('VEXE')
	args := os.args // args are: vpm SUBCOMMAND module names
	if args.len < 2 {
		vpm_help([])
		exit(5)
	}
	vpm_command := args[1]
	module_names := args[2..]
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
	if user_asks_for_help(keywords) {
		println('Usage:')
		println('  v search keyword1 [keyword2] [...]')
		println('  ^^^^^^^^^^^^^^^^^ will search https://vpm.vlang.io/ for matching modules,')
		println('                    and will show details about them')
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
	if user_asks_for_help(module_names) {
		println('Usage:')
		println('  v install module [module] [module] [...]')
		println('  ^^^^^^^^^^^^^ will install the modules you specified')
		exit(0)
	}
	if module_names.len == 0 {
		println('  v install requires *at least one* module name')
		exit(2)
	}
	mut errors := 0
	for n in module_names {
		name := n.trim_space()
		modurl := url + '/jsmod/$name'
		r := http.get(modurl) or {
			panic(err)
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
		final_module_path := get_vmodules_dir_path() + '/' + mod.name.replace('.', '/')
		if os.exists(final_module_path) {
			println('Skipping module "$name", since it already exists. Use "v update $name" to update it.')
			continue
		}
		println('Installing module "$name" from $mod.url to $final_module_path ...')
		os.exec('git clone --depth=1 $mod.url $final_module_path') or {
			errors++
			println('Could not install module "$name" to "$final_module_path" .')
			println('Error details: $err')
			continue
		}
		resolve_dependencies(name, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_update(module_names []string) {
	if user_asks_for_help(module_names) {
		println('Usage: ')
		println(' a) v update module [module] [module] [...]')
		println('    ^^^^^^^^^^^^ will update the listed modules to their latest versions')
		println(' b) v update')
		println('    ^^^^^^^^^^^^ will update ALL installed modules to their latest versions')
		exit(0)
	}
	if module_names.len == 0 {
		println('  v update requires *at least one* module name')
		exit(2)
	}
	mut errors := 0
	for name in module_names {
		final_module_path := get_vmodules_dir_path() + '/' + name.replace('.', '/')
		if !os.exists(final_module_path) {
			println('No module with name "$name" exists at $final_module_path')
			continue
		}
		os.chdir(final_module_path)
		println('Updating module "$name"...')
		os.exec('git pull --depth=1') or {
			errors++
			println('Could not update module "$name".')
			println('Error details: $err')
			continue
		}
		resolve_dependencies(name, final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_remove(module_names []string) {
	if user_asks_for_help(module_names) {
		println('Usage: ')
		println(' a) v remove module [module] [module] [...]')
		println('    ^^^^^^^^^^^^ will remove the listed modules')
		println(' b) v remove')
		println('    ^^^^^^^^^^^^ will remove ALL installed modules')
		exit(0)
	}
	if module_names.len == 0 {
		println('  v update requires *at least one* module name')
		exit(2)
	}
	mut errors := 0
	for name in module_names {
		final_module_path := get_vmodules_dir_path() + '/' + name.replace('.', '/')
		if !os.exists(final_module_path) {
			println('No module with name "$name" exists at $final_module_path')
			continue
		}
		println('Removing module "$name"...')
		// TODO os.rmdir for some reason doesn't work
		os.exec('rm -rf $final_module_path') or {
			errors++
			println('Could not remove module "$name".')
			println('Error details: $err')
			continue
		}
	}
	if errors > 0 {
		exit(1)
	}
}

fn get_vmodules_dir_path() string {
	return os.home_dir() + '.vmodules'
}

fn ensure_vmodules_dir_exist() {
	home_vmodules := get_vmodules_dir_path()
	if !os.is_dir(home_vmodules) {
		println('Creating $home_vmodules/ ...')
		os.mkdir(home_vmodules) or {
			panic(err)
		}
	}
}

fn change_to_vmodules_dir() {
	os.chdir(get_vmodules_dir_path())
}

fn todo(vpm_command string) {
	println('TODO: v $vpm_command')
	exit(4)
}

fn user_asks_for_help(module_names []string) bool {
	return ('-h' in module_names) || ('--help' in module_names) || ('help' in module_names)
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

fn get_all_modules() []string {
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
		vpm_install(deps)
	}
}

fn parse_vmod(data string) Vmod {
	keys := ['name', 'version', 'deps']
	mut m := map[string]string{}
	for key in keys {
		mut key_index := data.index('$key:') or {
			continue
		}
		key_index += key.len + 1
		m[key] = data[key_index..data.index_after('\n', key_index)].trim_space().replace("'", '').replace('[', '').replace(']', '')
	}
	mut vmod := Vmod{}
	if 'name' in m {
		vmod.name = m['name']
	}
	if 'version' in m {
		vmod.version = m['version']
	}
	if 'deps' in m && m['deps'].len > 0 {
		vmod.deps = m['deps'].split(',')
	}
	return vmod
}
