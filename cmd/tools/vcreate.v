// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This module follows a similar convention to Rust: `init` makes the
// structure of the program in the _current_ directory, while `new`
// makes the program structure in a _sub_ directory. Besides that, the
// functionality is essentially the same.
module main

import os

struct Create {
mut:
	name        string
	description string
}

fn cerror(e string){
	eprintln('\nerror: $e')
}

fn vmod_content(name, desc string) string {
	return  [
		'Module {',
		'	name: \'${name}\',',
		'	description: \'${desc}\',',
		'	dependencies: []',
		'}'
	].join('\n')
}

fn main_content() string {
	return [
		'module main\n',
		'fn main() {',
		'	println(\'Hello World !\')',
		'}'
	].join('\n')
}

fn gen_gitignore(name string) string {
	return [
		'main',
		'$name',
		'*.so',
		'*.dylib',
		'*.dll'
	].join('\n')
}

fn (c &Create)create_vmod() {
	mut vmod := os.create('${c.name}/v.mod') or {
		cerror(err)
		exit(1)
	}
	vmod.write(vmod_content(c.name, c.description))
	vmod.close()
}

fn (c &Create)create_main() {
	mut main := os.create('${c.name}/${c.name}.v') or {
		cerror(err)
		exit(2)
	}
	main.write(main_content())
	main.close()
}

fn (c &Create)init_vmod() {
	mut vmod := os.create('v.mod') or {
		cerror(err)
		exit(1)
	}
	vmod.write(vmod_content(c.name, c.description))
	vmod.close()
}

fn (c &Create)create_git_repo(dir string) {
	// Create Git Repo and .gitignore file
	if !os.is_dir('${dir}/.git') {
		os.exec('git init ${dir}') or {
			cerror('Unable to create git repo')
			exit(4)
		}
		if !os.exists('${dir}/.gitignore') {
			mut fl := os.create('${dir}/.gitignore') or {
				// We don't really need a .gitignore, it's just a nice-to-have
				return
			}
			fl.write(gen_gitignore(c.name))
			fl.close()
		}
	}
}

fn (c &Create)init_main() {
	// The file already exists, don't over-write anything.
	// Searching in the 'src' directory allows flexibility user module structure
	if os.exists('${c.name}.v') || os.exists('src/${c.name}.v') {
		return
	}
	mut main := os.create('${c.name}.v') or {
		cerror(err)
		exit(2)
	}
	main.write(main_content())
	main.close()
}

fn create() {
	mut c := Create{}

	print('Input your project name: ')
	c.name = os.get_line()

	if os.is_dir(c.name) {
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
	c.create_git_repo(c.name)
}

fn init() {
	if os.exists('v.mod') {
		cerror('`v init` cannot be run on existing v modules')
		exit(3)
	}
	mut c := Create{}
	c.name = os.file_name(os.getwd())
	c.description = ''
	c.init_vmod()
	c.init_main()
	c.create_git_repo('')
	println("Change your module's description in `v.mod`")
}

fn main() {
	if 'new' == os.args[1] {
		create()
	} else if 'init' == os.args[1] {
		init()
	} else {
		cerror('Unknown command: ${os.args[1]}')
		exit(1)
	}
	println('Complete!')
}
