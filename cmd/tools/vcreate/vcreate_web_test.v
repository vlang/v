module main

import os
import v.cmdexec

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
	assert main_v.contains('app.sessions.resave'), 'login must rotate the session id with resave'
	template_html := os.read_file(os.join_path(project_path, 'templates', 'index.html'))!
	assert template_html.contains('veb starter')
	// Build the generated app with the login probe so both checks share its web stack.
	os.write_file(os.join_path(project_path, 'login_probe.v'), $embed_file('tests/web_login_rotation.vv').to_string())!
	os.chdir(project_path)!
	binary := os.join_path(project_path, 'web_login_probe' + $if windows { '.exe' } $else { '' })
	res := os.exec([@VEXE, '-o', binary, '.'])
	assert res.exit_code == 0, res.output
	probe := cmdexec.run_with_timeout(binary, ['--check-web-login'], 5_000)
	assert probe.exit_code == 0, probe.output
	assert probe.output.trim_space() == 'web login rotation verified', probe.output
}
