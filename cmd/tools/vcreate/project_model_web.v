module main

import os

fn (mut c Create) set_web_project_files() {
	base := if c.new_dir { c.name } else { '' }
	c.files << ProjectFiles{
		path:    os.join_path(base, 'assets', 'main.css')
		content: "html, body {
	font-family: Arial, Helvetica, sans-serif;
	background: #f4efe6;
	color: #1f2933;
	height: 100%;
	margin: 0;
}

body {
	display: grid;
	place-items: center;
	padding: 24px;
}

.card {
	max-width: 560px;
	padding: 32px;
	border: 1px solid #d8ccb8;
	border-radius: 16px;
	background: #fffaf1;
	box-shadow: 0 18px 50px rgba(69, 52, 35, 0.08);
}

.eyebrow {
	margin: 0 0 12px;
	font-size: 12px;
	font-weight: 700;
	letter-spacing: 0.12em;
	text-transform: uppercase;
	color: #8b5e34;
}

h1 {
	margin: 0 0 12px;
	font-size: 40px;
	line-height: 1.1;
}

p {
	margin: 0;
	font-size: 18px;
	line-height: 1.6;
}

.hint {
	margin-top: 16px;
	font-size: 14px;
	color: #52606d;
}

code {
	font-family: 'Courier New', monospace;
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'templates', 'index.html')
		content: "<!doctype html>
<html lang='en'>
<head>
	<meta charset='UTF-8'>
	<meta name='viewport' content='width=device-width, initial-scale=1.0'>
	<title>@title</title>
	@css '/assets/main.css'
</head>
<body>
	<main class='card'>
		<p class='eyebrow'>veb starter</p>
		<h1>@title</h1>
		<p>@message</p>
		<p class='hint'>Try <code>GET /health</code>, then <code>POST /login</code> with a <code>name</code> field and <code>GET /me</code>.</p>
	</main>
</body>
</html>
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'main.v')
		content: "module main

import os
import veb
import x.sessions
import x.sessions.veb_middleware

pub struct User {
pub mut:
	name string
}

pub struct Context {
	veb.Context
	sessions.CurrentSession[User]
}

pub struct App {
	veb.Middleware[Context]
	veb.StaticHandler
mut:
	sessions sessions.Sessions[User]
}

pub fn (app &App) index() veb.Result {
	title := '${c.name}'
	message := 'Your new V web app is powered by veb.'
	return \$veb.html()
}

@['/health'; get]
pub fn (app &App) health(mut ctx Context) veb.Result {
	return ctx.text('ok')
}

@['/login'; post]
pub fn (mut app App) login(mut ctx Context, name string) veb.Result {
	if name == '' {
		ctx.res.set_status(.bad_request)
		return ctx.text('name is required')
	}
	app.sessions.save(mut ctx, User{
		name: name
	}) or {
		return ctx.server_error('cannot save session')
	}
	return ctx.text('hello \${name}')
}

@['/me'; get]
pub fn (app &App) me(mut ctx Context) veb.Result {
	user := ctx.session_data or {
		ctx.res.set_status(.unauthorized)
		return ctx.text('not signed in: POST your name to /login first')
	}
	return ctx.text('hello \${user.name}')
}

fn main() {
	// Keep asset and template paths stable for `v run .`.
	os.chdir(os.dir(@FILE))!
	// Sessions are signed with this secret. Generate a real one with
	// `openssl rand -hex 32` and never deploy the fallback below.
	secret := os.getenv_opt('SESSION_SECRET') or {
		eprintln('warning: SESSION_SECRET is not set, using an insecure dev secret')
		'dev-only-insecure-secret-change-me'
	}
	mut app := &App{
		sessions: sessions.Sessions[User]{
			store:  sessions.MemoryStore[User]{}
			secret: secret.bytes()
		}
	}
	app.use(veb_middleware.create[User, Context](mut app.sessions))
	app.handle_static('assets', false)!
	veb.run[App, Context](mut app, 8080)
}
"
	}
}
