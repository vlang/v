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

fn main() {
	if os.args.len <= 1 {
		println('usage: vget module [module] [module] [...]')
		return
	} 

	home := os.home_dir() 
	if !os.dir_exists(home + '/.vmodules') {
		println('Creating vmodules directory...') 
		os.chdir(home) 
		os.mkdir('.vmodules') 
		println('Done.') 
	} 

	names := os.args.slice(1, os.args.len)
	for name in names {
		s := http.get_text(url + '/jsmod/$name')
		mod := json.decode(Mod, s) or {
			println('Error. Make sure you are online.')
			return
		}
		
		println('Installing module ${name}...')
		_ := os.exec('git -C $home/.vmodules clone --depth=1 $mod.url ' + mod.name.replace('.', '/')) or {
			panic(err)
		}
		println(s)
	}
}
