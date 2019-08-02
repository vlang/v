module main

import (
	http 
	os 
	json
) 

const (
	//url = 'http://localhost:8089' 
	url = 'http://vpm.vlang.io' 
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
	mod := json.decode(Mod, s) or { return } 
	home := os.home_dir() 
	os.exec('git -C "$home/.vmodules" clone $mod.url') 
	println(s) 
} 

