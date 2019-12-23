module main

import (
	http
	os
	json
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
		}}
}

fn vpm_search(module_names []string) {
	if user_asks_for_help(module_names) {
		println('Usage:')
		println('  v search keyword1 [keyword2] [...]')
		println('  ^^^^^^^^^^^^^^^^^ will search https://vpm.vlang.io/ for matching modules,')
		println('                    and will show details about them')
		exit(0)
	}
	if module_names.len == 0 {
		println('  v search requires *at least one* keyword')
		exit(2)
	}
	todo('search')
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
	for name in module_names {
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
		println('Installing module "$name" from $mod.url to $final_module_path ...')
		_ = os.exec('git clone --depth=1 $mod.url $final_module_path') or {
			errors++
			println('Could not install module "$name" to "$final_module_path" .')
			println('Error details: $err')
			continue
		}
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
	todo('update')
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
	todo('remove')
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

