module main

import (
	http
	os
	json
)

const (
	//url = 'http://localhost:8089'
	url = 'https://vpm.best'
)

struct Mod {
	id int
	name string
	url string
	nr_downloads int
}

fn get_vmodules_dir_path() string {
	home := os.home_dir()

	return '${home}.vmodules'
}

fn ensure_vmodules_dir_exist() {
	home_vmodules := get_vmodules_dir_path()

	if !os.dir_exists( home_vmodules ) {
		println('Creating $home_vmodules/ ...')
		os.mkdir(home_vmodules)
	}
}

fn change_to_vmodules_dir() {
	os.chdir(get_vmodules_dir_path())
}

fn main() {
	if os.args.len <= 1 {
		println('usage: vget module [module] [module] [...]')
		exit(2)
	}

	ensure_vmodules_dir_exist()
	change_to_vmodules_dir()

	mut errors := 0
	names := os.args.slice(1, os.args.len)
	for name in names {
		modurl := url + '/jsmod/$name'
		r := http.get(modurl) or { panic(err) }

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
		mod := json.decode(Mod, s) or {
			errors++
			println('Skipping module "$name", since its information is not in json format.')
			continue
		}

		if( '' == mod.url || '' == mod.name ){
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
