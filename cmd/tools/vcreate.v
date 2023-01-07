// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os

// Note: this program follows a similar convention to Rust: `init` makes the
// structure of the program in the _current_ directory, while `new`
// makes the program structure in a _sub_ directory. Besides that, the
// functionality is essentially the same.

// Note: here are the currently supported invokations so far:
// 1) `v init` -> create a new project in the current folder
// 2) `v new abc` -> create a new project in the new folder `abc`, by default a "hello world" project.
// 3) `v new abcd web` -> create a new project in the new folder `abcd`, using the vweb template.
// 4) `v new abcde hello_world` -> create a new project in the new folder `abcde`, using the hello_world template.

// Note: run `v cmd/tools/vcreate_test.v` after changes to this program, to avoid regressions.

struct Create {
mut:
	name        string
	description string
	version     string
	license     string
	files       []ProjectFiles
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
			project_models := ['web', 'hello_world']
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

	c.description = if args.len > 1 { args[1] } else { os.input('Input your project description: ') }

	default_version := '0.0.0'
	c.version = os.input('Input your project version: (${default_version}) ')
	if c.version == '' {
		c.version = default_version
	}

	default_license := os.getenv_opt('VLICENSE') or { 'MIT' }
	c.license = os.input('Input your project license: (${default_license}) ')
	if c.license == '' {
		c.license = default_license
	}

	println('Initialising ...')
	match os.args.last() {
		'web' {
			c.set_web_project_files()
		}
		'hello_world' {
			c.set_hello_world_project_files()
		}
		else {
			eprintln('${os.args.last()} model not exist')
			exit(1)
		}
	}

	// gen project based in the `Create.files` info
	c.create_files_and_directories()

	c.write_vmod(true)
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

	c.set_hello_world_project_files()
	c.create_files_and_directories()
	c.write_gitattributes(false)
	c.write_editorconfig(false)
	c.create_git_repo('.')
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

fn hello_world_content() string {
	return "module main

fn main() {
	println('Hello World!')
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
indent_size = 4
'
}

fn index_template_content() string {
	return '@include "header.html"

Test <b>app</b>
<br>
<h1>@hello</h1>
<hr>

If demo: <br>
@if show
	show = true
@end

<br><br>

For loop demo: <br>

@for number in numbers
	@number <br>
@end


<hr>
End.
'
}

fn (c &Create) write_vmod(new bool) {
	vmod_path := if new { '${c.name}/v.mod' } else { 'v.mod' }
	os.write_file(vmod_path, vmod_content(c)) or { panic(err) }
}

fn (c &Create) write_gitattributes(new bool) {
	gitattributes_path := if new { '${c.name}/.gitattributes' } else { '.gitattributes' }
	if !new && os.exists(gitattributes_path) {
		return
	}
	os.write_file(gitattributes_path, gitattributes_content()) or { panic(err) }
}

fn (c &Create) write_editorconfig(new bool) {
	editorconfig_path := if new { '${c.name}/.editorconfig' } else { '.editorconfig' }
	if !new && os.exists(editorconfig_path) {
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
		dir := file.path.split(os.path_separator)#[..-1].join('/')
		// create all directories, if not exist
		os.mkdir_all(dir) or { panic(err) }
		os.write_file(file.path, file.content) or { panic(err) }
	}
}

fn (mut c Create) set_hello_world_project_files() {
	c.files << ProjectFiles{
		path: '${c.name}/src/main.v'
		content: hello_world_content()
	}
}

fn (mut c Create) set_web_project_files() {
	c.files << ProjectFiles{
		path: '${c.name}/src/databases/config_databases_sqlite.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/templates/header_component.html'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/templates/products.css'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/templates/products.html'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/auth_controllers.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/auth_dto.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/auth_services.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/index.html'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/main.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/product_controller.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/product_entities.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/product_service.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/product_view_api.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/product_view.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/user_controllers.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/user_entities.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/user_services.v'
		content: 'b'
	}
	c.files << ProjectFiles{
		path: '${c.name}/src/user_view_api.v'
		content: 'b'
	}
}
