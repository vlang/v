// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os

// Note: this program follows a similar convention to Rust: `init` makes the
// structure of the program in the _current_ directory, while `new`
// makes the program structure in a _sub_ directory. Besides that, the
// functionality is essentially the same.

// Note: here are the currently supported invocations so far:
// - `v init` -> initialize a new project in the current folder
// - `v new` -> create a new project in the directory specified during setup, using the "bin" template by default.
// - `v new my_bin_project bin` -> create a new project directory `my_bin_project`, using the bin template.
// - `v new my_lib_project lib` -> create a new project directory `my_lib_project`, using the lib template.
// - `v new my_web_project web` -> create a new project directory `my_web_project`, using the vweb template.

struct Create {
mut:
	name        string
	description string
	version     string
	license     string
	files       []ProjectFiles
	new_dir     bool
}

struct ProjectFiles {
	path    string
	content string
}

fn main() {
	cmd := os.args[1]
	match cmd {
		'new' {
			// list of models allowed
			project_models := ['bin', 'lib', 'web']
			if os.args.len == 4 {
				// validation
				if os.args.last() !in project_models {
					mut error_str := 'It is not possible create a "${os.args[os.args.len - 2]}" project.\n'
					error_str += 'See the list of allowed projects:\n'
					for model in project_models {
						error_str += 'v new ${os.args[os.args.len - 2]} ${model}\n'
					}
					eprintln(error_str)
					exit(1)
				}
			}
			new_project(os.args[2..])
		}
		'init' {
			init_project()
		}
		else {
			cerror('unknown command: ${cmd}')
			exit(1)
		}
	}
	println('Complete!')
}

fn new_project(args []string) {
	mut c := Create{}
	c.new_dir = true

	c.name = check_name(if args.len > 0 { args[0] } else { os.input('Input your project name: ') })

	if c.name == '' {
		cerror('project name cannot be empty')
		exit(1)
	}

	if c.name.contains('-') {
		cerror('"${c.name}" should not contain hyphens')
		exit(1)
	}

	if os.is_dir(c.name) {
		cerror('${c.name} folder already exists')
		exit(3)
	}

	c.prompt()

	println('Initialising ...')
	if args.len == 2 {
		// E.g.: `v new my_project lib`
		match os.args.last() {
			'bin' {
				c.set_bin_project_files()
			}
			'lib' {
				c.set_lib_project_files()
			}
			'web' {
				c.set_web_project_files()
			}
			else {
				eprintln('${os.args.last()} model not exist')
				exit(1)
			}
		}
	} else {
		// E.g.: `v new my_project`
		c.set_bin_project_files()
	}

	// gen project based in the `Create.files` info
	c.create_files_and_directories()

	c.write_vmod()
	c.write_gitattributes()
	c.write_editorconfig()
	c.create_git_repo(c.name)
}

fn init_project() {
	mut c := Create{}
	dir_name := check_name(os.file_name(os.getwd()))
	if !os.exists('v.mod') {
		mod_dir_has_hyphens := dir_name.contains('-')
		c.name = if mod_dir_has_hyphens { dir_name.replace('-', '_') } else { dir_name }
		c.prompt()
		c.write_vmod()
		if mod_dir_has_hyphens {
			println('The directory name `${dir_name}` is invalid as a module name. The module name in `v.mod` was set to `${c.name}`')
		}
	}
	if !os.exists('src/main.v') {
		c.set_bin_project_files()
	}
	c.create_files_and_directories()
	c.write_gitattributes()
	c.write_editorconfig()
	c.create_git_repo('.')
}

fn (mut c Create) prompt() {
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

fn cerror(e string) {
	eprintln('\nerror: ${e}')
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

fn vmod_content(c Create) string {
	return "Module {
	name: '${c.name}'
	description: '${c.description}'
	version: '${c.version}'
	license: '${c.license}'
	dependencies: []
}
"
}

fn gen_gitignore(name string) string {
	return '# Binaries for programs and plugins
main
${name}
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
}

fn gitattributes_content() string {
	return '* text=auto eol=lf
*.bat eol=crlf

**/*.v linguist-language=V
**/*.vv linguist-language=V
**/*.vsh linguist-language=V
**/v.mod linguist-language=V
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
'
}

fn (c &Create) write_vmod() {
	vmod_path := if c.new_dir { '${c.name}/v.mod' } else { 'v.mod' }
	os.write_file(vmod_path, vmod_content(c)) or { panic(err) }
}

fn (c &Create) write_gitattributes() {
	gitattributes_path := if c.new_dir { '${c.name}/.gitattributes' } else { '.gitattributes' }
	if !c.new_dir && os.exists(gitattributes_path) {
		return
	}
	os.write_file(gitattributes_path, gitattributes_content()) or { panic(err) }
}

fn (c &Create) write_editorconfig() {
	editorconfig_path := if c.new_dir { '${c.name}/.editorconfig' } else { '.editorconfig' }
	if !c.new_dir && os.exists(editorconfig_path) {
		return
	}
	os.write_file(editorconfig_path, editorconfig_content()) or { panic(err) }
}

fn (c &Create) create_git_repo(dir string) {
	// Create Git Repo and .gitignore file
	if !os.is_dir('${dir}/.git') {
		res := os.execute('git init ${dir}')
		if res.exit_code != 0 {
			cerror('Unable to create git repo')
			exit(4)
		}
	}
	gitignore_path := '${dir}/.gitignore'
	if !os.exists(gitignore_path) {
		os.write_file(gitignore_path, gen_gitignore(c.name)) or {}
	}
}

fn (mut c Create) create_files_and_directories() {
	for file in c.files {
		// get dir and convert path separator
		dir := file.path.split('/')#[..-1].join(os.path_separator)
		// create all directories, if not exist
		os.mkdir_all(dir) or { panic(err) }
		os.write_file(file.path, file.content) or { panic(err) }
	}
}
