// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This module follows a similar convention to Rust: `init` makes the
// structure of the program in the _current_ directory, while `create`
// makes the program structure in a _sub_ directory. Besides that, the
// functionality is essentially the same.
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

[inline]
fn vmod_content(name, desc string) string {
	return  [
		'#V Project#\n',
		'Module {',
		'	name: \'${name}\',',
		'	description: \'${desc}\',',
		'	dependencies: []',
		'}'
	].join('\n')
}

[inline]
fn main_content() string {
	return [
		'module main\n',
		'fn main() {',
		'	println(\'Hello World !\')',
		'}'
	].join('\n')
}

fn (c &Create)create_vmod() {
	mut vmod := os.create('${c.name}/v.mod') or {
		cerror(err)
		exit(1)
	}
	vmod.write(vmod_content(c.name, c.description))
}

fn (c &Create)create_main() {
	mut main := os.create('${c.name}/${c.name}.v') or {
		cerror(err)
		exit(2)
	}
	main.write(main_content())
}

fn (c &Create)init_vmod() {
	mut vmod := os.create('v.mod') or {
		cerror(err)
		exit(1)
	}
	vmod.write(vmod_content(c.name, c.description))
}

fn (c &Create)init_main() {
	// The file already exists, don't over-write anything.
	if os.exists('${c.name}.v') {
		return
	}
	mut main := os.create('${c.name}.v') or {
		cerror(err)
		exit(2)
	}
	main.write(main_content())
}

fn create() {
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
	c.create_vmod()
	c.create_main()
}

fn init() {
	if os.exists('v.mod') {
		cerror('`v init` cannot be run on existing v modules')
		exit(3)
	}
	mut c := Create{}
	c.name = os.filename(os.getwd())
	c.description = ''
	c.init_vmod()
	c.init_main()
}

fn main() {
	if 'create' == os.args[1] {
		create()
	} else if 'init' == os.args[1] {
		init()
	} else {
		cerror('Unknown command: ${os.args[1]}')
		exit(1)
	}
	println('Complete !')
}
