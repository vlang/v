// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

// This module follows a similar convention to Rust: `init` makes the
// structure of the program in the _current_ directory, while `new`
// makes the program structure in a _sub_ directory. Besides that, the
// functionality is essentially the same.
import os

struct Create {
mut:
	name        string
	description string
	version     string
	license     string
}

fn cerror(e string) {
	eprintln('\nerror: $e')
}

fn check_name(name string) string {
	if name.is_title() {
		mut cname := name.to_lower()
		if cname.contains(' ') {
			cname = cname.replace(' ', '_')
		}
		eprintln('warning: the project name cannot be capitalized, the name will be changed to `$cname`')
		return cname
	}
	if name.contains(' ') {
		cname := name.replace(' ', '_')
		eprintln('warning: the project name cannot contain spaces, the name will be changed to `$cname`')
		return cname
	}
	return name
}

fn vmod_content(c Create) string {
	return [
		'Module {',
		"	name: '$c.name'",
		"	description: '$c.description'",
		"	version: '$c.version'",
		"	license: '$c.license'",
		'	dependencies: []',
		'}',
	].join('\n')
}

fn main_content() string {
	return [
		'module main\n',
		'fn main() {',
		"	println('Hello World!')",
		'}',
	].join('\n')
}

fn gen_gitignore(name string) string {
	return [
		'# Binaries for programs and plugins',
		'main',
		'$name',
		'*.exe',
		'*.exe~',
		'*.so',
		'*.dylib',
		'*.dll',
	].join('\n')
}

fn (c &Create) write_vmod(new bool) {
	vmod_path := if new { '$c.name/v.mod' } else { 'v.mod' }
	mut vmod := os.create(vmod_path) or {
		cerror(err.msg)
		exit(1)
	}
	vmod.write_str(vmod_content(c)) or { panic(err) }
	vmod.close()
}

fn (c &Create) write_main(new bool) {
	if !new && (os.exists('${c.name}.v') || os.exists('src/${c.name}.v')) {
		return
	}
	main_path := if new { '$c.name/${c.name}.v' } else { '${c.name}.v' }
	mut mainfile := os.create(main_path) or {
		cerror(err.msg)
		exit(2)
	}
	mainfile.write_str(main_content()) or { panic(err) }
	mainfile.close()
}

fn (c &Create) create_git_repo(dir string) {
	// Create Git Repo and .gitignore file
	if !os.is_dir('$dir/.git') {
		res := os.execute('git init $dir')
		if res.exit_code != 0 {
			cerror('Unable to create git repo')
			exit(4)
		}
		if !os.exists('$dir/.gitignore') {
			mut fl := os.create('$dir/.gitignore') or {
				// We don't really need a .gitignore, it's just a nice-to-have
				return
			}
			fl.write_str(gen_gitignore(c.name)) or { panic(err) }
			fl.close()
		}
	}
}

fn create(args []string) {
	mut c := Create{}
	c.name = check_name(if args.len > 0 { args[0] } else { os.input('Input your project name: ') })
	if c.name == '' {
		cerror('project name cannot be empty')
		exit(1)
	}
	if c.name.contains('-') {
		cerror('"$c.name" should not contain hyphens')
		exit(1)
	}
	if os.is_dir(c.name) {
		cerror('$c.name folder already exists')
		exit(3)
	}
	c.description = if args.len > 1 { args[1] } else { os.input('Input your project description: ') }
	default_version := '0.0.0'
	c.version = os.input('Input your project version: ($default_version) ')
	if c.version == '' {
		c.version = default_version
	}
	default_license := 'MIT'
	c.license = os.input('Input your project license: ($default_license) ')
	if c.license == '' {
		c.license = default_license
	}
	println('Initialising ...')
	os.mkdir(c.name) or { panic(err) }
	c.write_vmod(true)
	c.write_main(true)
	c.create_git_repo(c.name)
}

fn init_project() {
	if os.exists('v.mod') {
		cerror('`v init` cannot be run on existing v modules')
		exit(3)
	}
	mut c := Create{}
	c.name = check_name(os.file_name(os.getwd()))
	c.description = ''
	c.write_vmod(false)
	c.write_main(false)
	c.create_git_repo('')
	println("Change your module's description in `v.mod`")
}

fn main() {
	cmd := os.args[1]
	match cmd {
		'new' {
			create(os.args[2..])
		}
		'init' {
			init_project()
		}
		else {
			cerror('unknown command: $cmd')
			exit(1)
		}
	}
	println('Complete!')
}
