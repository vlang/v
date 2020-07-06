module main

import cli
import os

struct Project {
mut:
	path string
	name string
	description string
	version string = '0.0.1'
	repository string
}

fn (p &Project) create_dir() {
	os.mkdir_all('${p.path}')
}

fn (p &Project) create_vmod() {
	if os.exists('${p.path}/v.mod') {
		return
	}

	vmod_content :=  [
		'Module {',
		'  name: \'${p.name}\',',
		'  description: \'${p.description}\',',
		'  version: \'${p.version}\',',
		'  repository: \'${p.repository}\','
		'  dependencies: [],',
		'}'
	].join('\n')

	mut vmod_file := os.create('${p.path}/v.mod') or {
		eprintln('\nerror: ${err}')
		exit(1)
	}
	vmod_file.write(vmod_content)
	vmod_file.close()
}

fn (p &Project) create_main() {
	if os.exists('${p.path}/${p.name}.v') || os.exists('${p.path}/src/${p.name}.v') {
		return
	}

	main_content := [
		'module main',
		'',
		'fn main() {',
		'  println(\'Hello World!\')',
		'}'
	].join('\n')
	
	mut main_file := os.create('${p.path}/${p.name}.v') or {
		eprintln('\nerror: ${err}')
		exit(1)
	}
	main_file.write(main_content)
	main_file.close()
}

fn (p &Project) create_git_repository() {
	if !os.is_dir('${p.path}/.git') {
		os.exec('git init ${p.path}') or {
			eprintln('\nerror: Unable to create git repository')
			exit(1)
		}
	}

	if !os.exists('${p.path}/.gitignore') {
		gitignore_content := [
			'# ignore compiled binaries',
			'${p.name}'
			'*.exe',
			'*.o',
			'*.so',
			'.*.c',
			'*.tmp.c',
			'*.obj',
			'*.exp',
			'*.ilk',
			'*.pdb',
			'*.dynlib'
			'*.dll',
			'*.lib',
			'*.bak',
			'a.out',
			'',
			'# ignore system files',
			'.DS_Store',
			'._*',
			'thumbs.db',
			'',
			'# ignore editor files',
			'.idea',
			'.project',
			'.classpath',
			'.c9',
			'*.launch',
			'.settings/',
			'*.sublime-workspace',
			'.vscode/',
			'*.code-workspace',
			'*~',
			'*.swp',
			'*.swo',
			'*.swn',
		].join('\n')

		mut gitignore_file := os.create('${p.path}/.gitignore') or {
			eprintln('\nerror: ${err}')
			exit(1)
		}
		gitignore_file.write(gitignore_content)
		gitignore_file.close()
	}
}

fn init_cmd() cli.Command {
	mut v_init_cmd := cli.Command{
		name: 'init'
		description: 'Setup file structure for a V project'
		execute: fn(cmd cli.Command) {
			mut p := Project{}

			if cmd.args.len >= 1 {
				p.path = cmd.args[0]
			}
			p.path = os.real_path(p.path)

			if os.exists('${p.path}/v.mod')	{
				eprintln('\nerror: A V project is already existing')
				exit(1)
			}

			p.name = os.file_name(os.getwd() + '/' + p.path)
			print('Project name (default: ${p.name}): ')
			name := os.get_line()
			if p.name != name && name.len > 0 {
				p.name = name
			}

			print('Project description: ')
			p.description = os.get_line()

			print('Project version (default: ${p.version}): ')
			version := os.get_line()
			if p.version != version && version.len > 0 {
				p.version = version
			}

			print('Project repository: ')
			p.repository = os.get_line()

			p.create_dir()
			p.create_vmod()
			p.create_main()
			p.create_git_repository()
		}
	}

	return v_init_cmd
}