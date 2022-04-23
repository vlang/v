// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
	if name.trim_space().len == 0 {
		cerror('project name cannot be empty')
		exit(1)
	}
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
	return "Module {
	name: '$c.name'
	description: '$c.description'
	version: '$c.version'
	license: '$c.license'
	dependencies: []
}
"
}

fn main_content() string {
	return "module main

fn main() {
	println('Hello World!')
}
"
}

fn gen_gitignore(name string) string {
	return '# Binaries for programs and plugins
main
$name
*.exe
*.exe~
*.so
*.dylib
*.dll
vls.log
'
}

fn gitattributes_content() string {
	return '*.v linguist-language=V text=auto eol=lf
*.vv linguist-language=V text=auto eol=lf
*.vsh linguist-language=V text=auto eol=lf
**/v.mod linguist-language=V text=auto eol=lf
'
}

fn editorconfig_content() string {
	return '[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.v]
indent_style = tab
indent_size = 4
'
}

fn (c &Create) write_vmod(new bool) {
	vmod_path := if new { '$c.name/v.mod' } else { 'v.mod' }
	os.write_file(vmod_path, vmod_content(c)) or { panic(err) }
}

fn (c &Create) write_main(new bool) {
	if !new && (os.exists('${c.name}.v') || os.exists('src/${c.name}.v')) {
		return
	}
	main_path := if new { '$c.name/${c.name}.v' } else { '${c.name}.v' }
	os.write_file(main_path, main_content()) or { panic(err) }
}

fn (c &Create) write_gitattributes(new bool) {
	gitattributes_path := if new { '$c.name/.gitattributes' } else { '.gitattributes' }
	if !new && os.exists(gitattributes_path) {
		return
	}
	os.write_file(gitattributes_path, gitattributes_content()) or { panic(err) }
}

fn (c &Create) write_editorconfig(new bool) {
	editorconfig_path := if new { '$c.name/.editorconfig' } else { '.editorconfig' }
	if !new && os.exists(editorconfig_path) {
		return
	}
	os.write_file(editorconfig_path, editorconfig_content()) or { panic(err) }
}

fn (c &Create) create_git_repo(dir string) {
	// Create Git Repo and .gitignore file
	if !os.is_dir('$dir/.git') {
		res := os.execute('git init $dir')
		if res.exit_code != 0 {
			cerror('Unable to create git repo')
			exit(4)
		}
	}
	gitignore_path := '$dir/.gitignore'
	if !os.exists(gitignore_path) {
		os.write_file(gitignore_path, gen_gitignore(c.name)) or {}
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
	default_license := os.getenv_opt('VLICENSE') or { 'MIT' }
	c.license = os.input('Input your project license: ($default_license) ')
	if c.license == '' {
		c.license = default_license
	}
	println('Initialising ...')
	os.mkdir(c.name) or { panic(err) }
	c.write_vmod(true)
	c.write_main(true)
	c.write_gitattributes(true)
	c.write_editorconfig(true)
	c.create_git_repo(c.name)
}

fn init_project() {
	mut c := Create{}
	c.name = check_name(os.file_name(os.getwd()))
	if !os.exists('v.mod') {
		c.description = ''
		c.write_vmod(false)
		println('Change the description of your project in `v.mod`')
	}
	c.write_main(false)
	c.write_gitattributes(false)
	c.write_editorconfig(false)
	c.create_git_repo('.')
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
