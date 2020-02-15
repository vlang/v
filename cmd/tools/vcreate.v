// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	os
)

struct Create {
mut:
	name        string
	description string
}

fn cerror(e string){
	eprintln('\nerror: $e')
}

fn (c Create)write_vmod() {
	mut vmod := os.create('${c.name}/v.mod') or {
		cerror(err)
		exit(1)
	}
	vmod_content := [
		'#V Project#\n',
		'Module {',
		'	name: \'${c.name}\',',
		'	description: \'${c.description}\',',
		'	dependencies: []',
		'}'
	]
	vmod.write(vmod_content.join('\n'))
}

fn (c Create)write_main() {
	mut main := os.create('${c.name}/${c.name}.v') or {
		cerror(err)
		exit(2)
	}
	main_content := [
		'module main\n',
		'fn main() {',
		'	println(\'Hello World !\')',
		'}'
	]
	main.write(main_content.join('\n'))
}

fn main() {
	mut c := Create{}

	print('Input your project name: ')
	c.name = os.get_line()
	
	if (os.is_dir(c.name)) {
		cerror('${c.name} folder already exists')
		exit(3)
	}

	print('Input your project description: ')
	c.description = os.get_line()

	println('Initialising ...')

	os.mkdir(c.name) or {
		panic(err)
	}
	c.write_vmod()
	c.write_main()
	println('Complete !')
}
