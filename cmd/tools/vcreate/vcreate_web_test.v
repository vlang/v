module main

import os

const web_test_path = os.join_path(os.vtmp_dir(), 'test_vcreate_web')

fn test_web_template_uses_veb() {
	os.rmdir_all(web_test_path) or {}
	defer {
		os.rmdir_all(web_test_path) or {}
	}
	os.mkdir_all(web_test_path)!
	old_wd := os.getwd()
	defer {
		os.chdir(old_wd) or {}
	}
	os.chdir(web_test_path)!
	project_name := 'my_web_project'
	project_path := os.join_path(web_test_path, project_name)
	mut c := Create{
		name:        project_name
		description: 'My Awesome V Web Project.'
		version:     '0.1.0'
		license:     'MIT'
		new_dir:     true
		template:    .web
	}
	c.create_files_and_directories()
	c.write_vmod()
	main_v := os.read_file(os.join_path(project_path, 'main.v'))!
	assert main_v.contains('import veb')
	assert main_v.contains('\$veb.html()')
	assert main_v.contains('x.sessions'), 'the web template must wire sessions'
	assert main_v.contains('SESSION_SECRET'), 'the web template must read its session secret from the environment'
	assert main_v.contains('/login')
	assert main_v.contains('/me')
	template_html := os.read_file(os.join_path(project_path, 'templates', 'index.html'))!
	assert template_html.contains('veb starter')
	os.chdir(project_path)!
	res := os.exec([@VEXE, '.'])
	assert res.exit_code == 0, res.output
}
