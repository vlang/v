// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import cli { Command, Flag }

// Note: this program follows a similar convention as Rust cargo:
// `init` creates the structure of project in the current directory,
// `new` creates the structure of a project in a sub directory.

struct Create {
mut:
	name        string
	description string
	version     string
	license     string
	files       []ProjectFiles
	new_dir     bool
	template    Template
}

struct ProjectFiles {
	path    string
	content string
}

enum Template {
	bin
	lib
	web
}

fn main() {
	flags := [
		Flag{
			flag:        .bool
			name:        'bin'
			description: 'Use the template for an executable application [default].'
		},
		Flag{
			flag:        .bool
			name:        'lib'
			description: 'Use the template for a library project.'
		},
		Flag{
			flag:        .bool
			name:        'web'
			description: 'Use the template for a vweb project.'
		},
	]
	mut cmd := Command{
		flags:      [
			Flag{
				flag:        .bool
				name:        'help'
				description: 'Print help information.'
				global:      true
			},
		]
		posix_mode: true
		commands:   [
			Command{
				name:        'new'
				usage:       '<project_name>'
				description: [
					'Creates a new V project in a directory with the specified project name.',
					'',
					'A setup prompt is started to create a `v.mod` file with the projects metadata.',
					'The <project_name> argument can be omitted and entered in the prompts dialog.',
					'If git is installed, `git init` will be performed during the setup.',
				].join_lines()
				parent:      &Command{
					name: 'v'
				}
				posix_mode:  true
				flags:       flags
				pre_execute: validate
				execute:     new_project
			},
			Command{
				name:        'init'
				description: [
					'Sets up a V project within the current directory.',
					'',
					"If no `v.mod` exists, a setup prompt is started to create one with the project's metadata.",
					'If no `.v` file exists, a project template is generated. If the current directory is not a',
					'git project and git is installed, `git init` will be performed during the setup.',
				].join_lines()
				parent:      &Command{
					name: 'v'
				}
				posix_mode:  true
				flags:       flags
				pre_execute: validate
				execute:     init_project
			},
		]
	}
	cmd.parse(os.args)
}

fn validate(cmd Command) ! {
	if cmd.flags.get_bool('help')! {
		cmd.execute_help()
		exit(0)
	}
	if cmd.args.len > 1 {
		cerror('too many arguments.\n')
		cmd.execute_help()
		exit(2)
	}
}

fn new_project(cmd Command) ! {
	mut c := Create{
		template: get_template(cmd)
		new_dir:  true
	}
	c.prompt(cmd.args)
	println('Initialising ...')
	// Generate project files based on `Create.files`.
	c.create_files_and_directories()
	c.write_vmod()
	c.write_gitattributes()
	c.write_editorconfig()
	c.create_git_repo(c.name)
}

fn init_project(cmd Command) ! {
	mut c := Create{
		template: get_template(cmd)
	}
	dir_name := check_name(os.file_name(os.getwd()))
	if !os.exists('v.mod') {
		mod_dir_has_hyphens := dir_name.contains('-')
		c.name = if mod_dir_has_hyphens { dir_name.replace('-', '_') } else { dir_name }
		c.prompt(cmd.args)
		c.write_vmod()
		if mod_dir_has_hyphens {
			println('The directory name `${dir_name}` is invalid as a module name. The module name in `v.mod` was set to `${c.name}`')
		}
	}
	println('Initialising ...')
	c.create_files_and_directories()
	c.write_gitattributes()
	c.write_editorconfig()
	c.create_git_repo('.')
}

fn (mut c Create) prompt(args []string) {
	if c.name == '' {
		c.name = check_name(args[0] or { os.input('Input your project name: ') })
		if c.name == '' {
			eprintln('')
			cerror('project name cannot be empty')
			exit(1)
		}
		if c.name.contains('-') {
			eprintln('')
			cerror('`${c.name}` should not contain hyphens')
			exit(1)
		}
		if os.is_dir(c.name) {
			eprintln('')
			cerror('`${c.name}` folder already exists')
			exit(3)
		}
	}
	c.description = os.input('Input your project description: ')
	default_version := '0.0.0'
	c.version = os.input('Input your project version: (${default_version}) ')
	if c.version == '' {
		c.version = default_version
	}
	default_license := 'MIT'
	c.license = os.input('Input your project license: (${default_license}) ')
	if c.license == '' {
		c.license = default_license
	}
}

fn get_template(cmd Command) Template {
	bin := cmd.flags.get_bool('bin') or { false }
	lib := cmd.flags.get_bool('lib') or { false }
	web := cmd.flags.get_bool('web') or { false }
	if (bin && lib) || (bin && web) || (lib && web) {
		eprintln("error: can't use more then one template")
		exit(2)
	}
	return match true {
		lib { .lib }
		web { .web }
		else { .bin }
	}
}

fn cerror(e string) {
	eprintln('error: ${e}.')
}

fn check_name(name string) string {
	if name.trim_space().len == 0 {
		eprintln('')
		cerror('project name cannot be empty')
		exit(1)
	}
	if name.is_title() {
		mut cname := name.to_lower()
		if cname.contains(' ') {
			cname = cname.replace(' ', '_')
		}
		eprintln('warning: the project name cannot be capitalized, the name will be changed to `${cname}`')
		return cname
	}
	if name.contains(' ') {
		cname := name.replace(' ', '_')
		eprintln('warning: the project name cannot contain spaces, the name will be changed to `${cname}`')
		return cname
	}
	return name
}

fn (c &Create) write_vmod() {
	path := if c.new_dir { '${c.name}/v.mod' } else { 'v.mod' }
	content := "Module {
	name: '${c.name}'
	description: '${c.description}'
	version: '${c.version}'
	license: '${c.license}'
	dependencies: []
}
"
	os.write_file(path, content) or { panic(err) }
}

fn (c &Create) write_gitattributes() {
	path := if c.new_dir { '${c.name}/.gitattributes' } else { '.gitattributes' }
	if !c.new_dir && os.exists(path) {
		return
	}
	content := '* text=auto eol=lf
*.bat eol=crlf

*.v linguist-language=V
*.vv linguist-language=V
*.vsh linguist-language=V
v.mod linguist-language=V
.vdocignore linguist-language=ignore
'
	os.write_file(path, content) or { panic(err) }
}

fn (c &Create) write_editorconfig() {
	path := if c.new_dir { '${c.name}/.editorconfig' } else { '.editorconfig' }
	if !c.new_dir && os.exists(path) {
		return
	}
	content := '[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.v]
indent_style = tab
'
	os.write_file(path, content) or { panic(err) }
}

fn (c &Create) create_git_repo(dir string) {
	// Initialize git and add a .gitignore file.
	if !os.is_dir('${dir}/.git') {
		res := os.execute('git init ${dir}')
		if res.exit_code != 0 {
			eprintln('')
			cerror('unable to initialize a git repository')
			exit(4)
		}
	}
	ignore_path := '${dir}/.gitignore'
	if os.exists(ignore_path) {
		return
	}
	ignore_content := '# Binaries for programs and plugins
main
${c.name}
*.exe
*.exe~
*.so
*.dylib
*.dll

# Ignore binary output folders
bin/

# Ignore common editor/system specific metadata
.DS_Store
.idea/
.vscode/
*.iml

# ENV
.env

# vweb and database
*.db
*.js
'
	os.write_file(ignore_path, ignore_content) or {}
}

fn (mut c Create) create_files_and_directories() {
	// Set project template files for `v new` or when no `.v` files exists during `v init`.
	if c.new_dir || os.walk_ext('.', '.v').len == 0 {
		match c.template {
			.bin { c.set_bin_project_files() }
			.lib { c.set_lib_project_files() }
			.web { c.set_web_project_files() }
		}
	}
	for file in c.files {
		os.mkdir_all(os.dir(file.path)) or { panic(err) }
		os.write_file(file.path, file.content) or { panic(err) }
	}
	kind := match c.template {
		.bin { 'binary (application)' }
		.lib { 'library' }
		.web { 'web' }
	}
	println('Created ${kind} project `${c.name}`')
}
