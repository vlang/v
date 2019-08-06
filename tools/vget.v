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
	if os.args.len != 2 {
		println('usage: vget [module]') 
		return 
	} 
	name := os.args.last() 
	s := http.get_text(url + '/jsmod/$name') 
	mod := json.decode(Mod, s) or { 
		println('Error. Make sure you are online.') 
		return
	} 
	home := os.home_dir() 
    if !os.dir_exists(home + '/.vmodules') {
	println('Creating vmodules directory...') 
	os.chdir(home) 
	os.mkdir('.vmodules') 
	println('Done.') 
	} 
	os.exec('git -C "$home/.vmodules" clone --depth=1 $mod.url $mod.name')
	println(s) 
} 
