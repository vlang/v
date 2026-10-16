module main

import net.http
import os
import veb
import x.sessions

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
	os.chdir(project_path)!
	res := os.exec([@VEXE, '.'])
	assert res.exit_code == 0, res.output
}

struct WebLoginUser {
pub mut:
	name string
}

struct WebLoginContext {
	veb.Context
	sessions.CurrentSession[WebLoginUser]
}

fn web_login_cookie_value(ctx &WebLoginContext) string {
	for cookie in ctx.res.cookies() {
		if cookie.name == 'sid' {
			return cookie.value
		}
	}
	return ''
}

fn test_web_template_login_rotates_session_id() {
	secret := 'web-login-rotation-test-secret'.bytes()
	mut s := sessions.Sessions[WebLoginUser]{
		store:  sessions.MemoryStore[WebLoginUser]{}
		secret: secret
	}
	// First login issues a cookie, like POST /login without one.
	mut first_ctx := WebLoginContext{}
	s.resave(mut first_ctx, WebLoginUser{
		name: 'alice'
	})!
	first_cookie := web_login_cookie_value(&first_ctx)
	assert first_cookie != ''
	first_sid, first_valid := sessions.verify_session_id(first_cookie, secret)
	assert first_valid
	// A second login presenting the first cookie must rotate the cookie.
	mut second_ctx := WebLoginContext{}
	second_ctx.req.add_cookie(http.Cookie{
		name:  'sid'
		value: first_cookie
	})
	s.resave(mut second_ctx, WebLoginUser{
		name: 'bob'
	})!
	second_cookie := web_login_cookie_value(&second_ctx)
	assert second_cookie != ''
	assert second_cookie != first_cookie
	second_sid, second_valid := sessions.verify_session_id(second_cookie, secret)
	assert second_valid
	assert second_sid != first_sid
	// The old session is destroyed.
	if _ := s.store.get(first_sid, 0) {
		assert false, 'old session must be destroyed on re-login'
	}
	assert s.store.get(second_sid, 0)! == WebLoginUser{
		name: 'bob'
	}
	// The previous cookie no longer accesses the new session.
	mut stale_ctx := WebLoginContext{}
	stale_ctx.req.add_cookie(http.Cookie{
		name:  'sid'
		value: first_cookie
	})
	if _ := s.get(stale_ctx) {
		assert false, 'previous cookie must not access the new session'
	}
	mut fresh_ctx := WebLoginContext{}
	fresh_ctx.req.add_cookie(http.Cookie{
		name:  'sid'
		value: second_cookie
	})
	assert s.get(fresh_ctx)! == WebLoginUser{
		name: 'bob'
	}
}
