module main

import os

fn (mut c Create) set_web_project_files() {
	base := if c.new_dir { c.name } else { '' }

	// v source code

	c.files << ProjectFiles{
		path:    os.join_path(base, 'main.v')
		content: "module main

import veb
import db.sqlite
import os

struct Context {
	veb.Context
}

struct App {
	veb.StaticHandler
	db sqlite.DB
}

pub fn (app &App) index(mut ctx Context) veb.Result {
	title := 'veb app'
	return \$veb.html()
}

fn main() {
	mut db := sqlite.connect(os.resource_abs_path('app.db'))!
	sql db {
		create table User
		create table Product
	}!
	defer { db.close() or { panic(err) } }

	mut app := &App{
		db: db
	}
	app.handle_static(os.resource_abs_path('static'), true)!
	veb.run[App, Context](mut app, 8082)
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'user_model.v')
		content: "module main

@[table: 'users']
pub struct User {
mut:
	id       int    @[primary; sql: serial]
	username string @[unique]
	password string
	active   bool
	products []Product @[fkey: 'user_id']
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'user_view.v')
		content: "module main

import veb

@['/api/users'; get]
pub fn (app &App) api_get_users(mut ctx Context) veb.Result {
	token := ctx.req.header.get_custom('token') or { '' }
	if !app.verify_auth(token) {
		ctx.res.set_status(.unauthorized)
		return ctx.text('Not valid token')
	}
	response := app.get_users() or {
		ctx.res.set_status(.bad_request)
		return ctx.text('\${err}')
	}
	return ctx.json(response)
}

@['/api/user'; get]
pub fn (app &App) api_get_user(mut ctx Context) veb.Result {
	token := ctx.req.header.get_custom('token') or { '' }
	user_id := app.user_auth(token) or {
		ctx.res.set_status(.unauthorized)
		return ctx.text('\${err}')
	}
	response := app.get_user(user_id) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('\${err}')
	}
	return ctx.json(response)
}

@['/api/user/create'; post]
pub fn (app &App) controller_create_user(mut ctx Context) veb.Result {
	username := ctx.form['username'] or { '' }
	password := ctx.form['password'] or { '' }
	if username == '' {
		ctx.res.set_status(.bad_request)
		return ctx.text('username cannot be empty')
	}
	if password == '' {
		ctx.res.set_status(.bad_request)
		return ctx.text('password cannot be empty')
	}
	app.add_user(username, password) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('\${err}')
	}
	ctx.res.set_status(.created)
	return ctx.text('User created successfully')
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'user_controller.v')
		content: "module main

import crypto.bcrypt

fn (app &App) add_user(username string, password string) ! {
	hashed_password := bcrypt.generate_from_password(password.bytes(), bcrypt.min_cost)!
	user_model := User{
		username: username
		password: hashed_password
		active:   true
	}
	sql app.db {
		insert user_model into User
	}!
}

fn (app &App) get_users() ![]User {
	return sql app.db {
		select from User
	}!
}

fn (app &App) get_user(id int) !User {
	results := sql app.db {
		select from User where id == id
	}!
	if results.len == 0 {
		return error('no results')
	}
	return results[0]
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'auth_model.v')
		content: "module main

import time
import crypto.hmac
import encoding.base64
import crypto.sha256
import json

struct JwtHeader {
	alg string
	typ string
}

struct JwtPayload {
	sub         string    // (subject) = Entity to whom the token belongs, usually the user ID;
	iss         string    // (issuer) = Token issuer;
	exp         string    // (expiration) = Timestamp of when the token will expire;
	iat         time.Time // (issued at) = Timestamp of when the token was created;
	aud         string    // (audience) = Token recipient, represents the application that will use it.
	name        string
	roles       string
	permissions string
}

fn make_token(user User) string {
	secret := 'SECRET_KEY' // os.getenv('SECRET_KEY')
	jwt_header := JwtHeader{'HS256', 'JWT'}
	jwt_payload := JwtPayload{
		sub:  '\${user.id}'
		name: '\${user.username}'
		iat:  time.now()
	}
	header := base64.url_encode(json.encode(jwt_header).bytes())
	payload := base64.url_encode(json.encode(jwt_payload).bytes())
	signature := base64.url_encode(hmac.new(secret.bytes(), '\${header}.\${payload}'.bytes(),
		sha256.sum, sha256.block_size).bytestr().bytes())
	jwt := '\${header}.\${payload}.\${signature}'
	return jwt
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'auth_view.v')
		content: "module main

import veb

@['/api/auth'; post]
pub fn (app &App) api_auth(mut ctx Context) veb.Result {
	username := ctx.form['username'] or { '' }
	password := ctx.form['password'] or { '' }
	response := app.do_auth(username, password) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('error: \${err}')
	}
	return ctx.json(response)
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'auth_controller.v')
		content: "module main

import crypto.bcrypt
import crypto.hmac
import encoding.base64
import crypto.sha256
import json

fn (app &App) do_auth(username string, password string) !string {
	users := sql app.db {
		select from User where username == username
	}!
	if users.len == 0 {
		return error('user not found')
	}
	user := users.first()
	if !user.active {
		return error('user is not active')
	}
	bcrypt.compare_hash_and_password(password.bytes(), user.password.bytes()) or {
		return error('Failed to auth user, \${err}')
	}
	return make_token(user)
}

fn (app &App) verify_auth(token string) bool {
	if token == '' {
		return false
	}
	secret := 'SECRET_KEY' // os.getenv('SECRET_KEY')
	token_split := token.split('.')
	signature_mirror := hmac.new(secret.bytes(), '\${token_split[0]}.\${token_split[1]}'.bytes(),
		sha256.sum, sha256.block_size).bytestr().bytes()
	signature_from_token := base64.url_decode(token_split[2])
	return hmac.equal(signature_from_token, signature_mirror)
}

fn (app &App) user_auth(token string) !int {
	if !app.verify_auth(token) {
		return error('Invalid token')
	}
	jwt_payload_stringify := base64.url_decode_str(token.split('.')[1])
	jwt_payload := json.decode(JwtPayload, jwt_payload_stringify) or { return err }
	return jwt_payload.sub.int()
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'product_model.v')
		content: "module main

@[table: 'products']
struct Product {
	id         int @[primary; sql: serial]
	user_id    int
	name       string @[sql_type: 'TEXT']
	created_at string @[default: 'CURRENT_TIMESTAMP']
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'product_view.v')
		content: "module main

import veb

@['/products'; get]
pub fn (app &App) products(mut ctx Context) veb.Result {
	token := ctx.get_cookie('token') or {
		ctx.res.set_status(.bad_request)
		return ctx.text('Cookie not found')
	}
	user_id := app.user_auth(token) or {
		ctx.res.set_status(.unauthorized)
		return ctx.text('\${err}')
	}
	user := app.get_user(user_id) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('\${err}')
	}
	return \$veb.html()
}

@['/api/products'; get]
pub fn (app &App) api_get_products(mut ctx Context) veb.Result {
	token := ctx.req.header.get_custom('token') or { '' }
	user_id := app.user_auth(token) or {
		ctx.res.set_status(.unauthorized)
		return ctx.text('\${err}')
	}
	response := app.get_user_products(user_id) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('\${err}')
	}
	return ctx.json(response)
}

@['/api/product/create'; post]
pub fn (app &App) api_create_product(mut ctx Context) veb.Result {
	product_name := ctx.form['product_name'] or { '' }
	if product_name == '' {
		ctx.res.set_status(.bad_request)
		return ctx.text('product name cannot be empty')
	}
	token := ctx.req.header.get_custom('token') or { '' }
	user_id := app.user_auth(token) or {
		ctx.res.set_status(.unauthorized)
		return ctx.text('\${err}')
	}
	app.add_product(product_name, user_id) or {
		ctx.res.set_status(.bad_request)
		return ctx.text('\${err}')
	}
	ctx.res.set_status(.created)
	return ctx.text('product created successfully')
}
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'product_controller.v')
		content: 'module main

fn (app &App) add_product(product_name string, user_id int) ! {
	product_model := Product{
		name:    product_name
		user_id: user_id
	}
	sql app.db {
		insert product_model into Product
	}!
}

fn (app &App) get_user_products(user_id int) ![]Product {
	return sql app.db {
		select from Product where user_id == user_id
	}!
}
'
	}

	// html content

	c.files << ProjectFiles{
		path:    os.join_path(base, 'templates', 'index.html')
		content: "<!DOCTYPE html>
<html>
<head>
	<!--Let browser know website is optimized for mobile-->
	<meta charset='UTF-8' name='viewport' content='width=device-width, initial-scale=1.0'>
	<!-- Compiled and minified CSS -->
	<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css'>
	<!-- Compiled and minified JavaScript -->
	<script src='https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js'></script>
	<!-- Material UI icons -->
	<link href='https://fonts.googleapis.com/icon?family=Material+Icons' rel='stylesheet'>
	<title>\${title}</title>
</head>
<body>
	<div>@include 'layout/header.html'</div>
	<div  class='card-panel center-align' style='max-width: 240px; padding: 10px; margin: 10px; border-radius: 5px;'>
		<form id='index_form' method='post' action=''>
			<div style='display:flex; flex-direction: column;'>
				<input type='text' name='username' placeholder='Username' required autofocus>
				<input type='password' name='password' placeholder='Password' required>
			</div>
			<div style='margin-top: 10px;'>
				<input class='waves-effect waves-light btn-small' type='submit' onclick='login()' formaction='javascript:void(0);' value='Login'>
				<input class='waves-effect waves-light btn-small' type='submit' onclick='addUser()' formaction='javascript:void(0);' value='Register'>
			</div>
		</form>
	</div>
	<script type='text/javascript'>
		// function eraseCookie(name) {
		//     document.cookie = name + '=; Max-Age=0'
		// }
		async function addUser() {
			const form = document.querySelector('#index_form');
			const formData = new FormData(form);
			await fetch('/api/user/create', {
				method: 'POST',
				body: formData
			})
				.then( async (response) => {
					if (response.status !== 201) {
						throw await response.text()
					}
					return await response.text()
				})
				.then((data) => {
					alert('User created successfully')
				})
				.catch((error) => {
					alert(error);
				});
		}
		async function login() {
			const form = document.querySelector('#index_form');
			const formData = new FormData(form);
			await fetch('/api/auth', {
				method: 'POST',
				body: formData
			})
				.then( async (response) => {
					if (response.status !== 200) {
						throw await response.text()
					}
					return response.json()
				})
				.then((data) => {
					document.cookie = 'token='+data+';';
					window.location.href = '/products'
				})
				.catch((error) => {
					alert(error);
				});
		}
	</script>
</body>
</html>
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'templates', 'products.html')
		content: "<!DOCTYPE html>
<html>
<head>
	<!--Let browser know website is optimized for mobile-->
	<meta charset='UTF-8' name='viewport' content='width=device-width, initial-scale=1.0'>

	<!-- Compiled and minified CSS -->
	<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css'>

	<!-- Compiled and minified JavaScript -->
	<script src='https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js'></script>

	<!-- Material UI icons -->
	<link href='https://fonts.googleapis.com/icon?family=Material+Icons' rel='stylesheet'>

    @css '/css/products.css'

	<title>Login</title>
</head>
<body>
	<div>@include 'layout/header.html'</div>
	<!-- <button onclick='document.location.reload(true)'>Lala</button> -->
	<form id='product_form' method='post' action=''>
		<div class='row'>
			<div class='input-field col s2'>
			<input id='product_name' name='product_name'  type='text' class='validate'>
			<label class='active' for='product_name'>product name</label>
			</div>
			<div style='margin-top: 10px;'>
			 <input class='waves-effect waves-light btn-small' type='submit' onclick='addProduct()' formaction='javascript:void(0);' value='Register' required autofocus>
			</div>
		</div>
		<!-- <div style='width: 20; height: 300;'>
			<input type='text' name='product_name' placeholder='product name' required autofocus>
		</div> -->
	</form>
	<script type='text/javascript'>
		function getCookie(cookieName) {
			let cookie = {};
			document.cookie.split(';').forEach(function(el) {
				let [key,value] = el.split('=');
				cookie[key.trim()] = value;
			})
			return cookie[cookieName];
		}
		async function addProduct() {
			const form = document.querySelector('#product_form');
			const formData = new FormData(form);
			console.log(getCookie('token'));
			await fetch('/api/product/create', {
				method: 'POST',
				body: formData,
				headers :{
					token: getCookie('token')
				}
			})
			.then( async (response) => {
				if (response.status !== 201) {
					throw await response.text()
				}
				return await response.text()
			})
			.then((data) => {
				//  alert('Product created successfully')
				document.location.reload()
			})
			.catch((error) => {
				alert(error);
			});
		}
	</script>
	<div class='products-table card-panel'>
		<table class='highlight striped responsive-table'>
			<thead>
			<tr>
				<th>ID</th>
				<th>Name</th>
				<th>Created date</th>
			</tr>
			</thead>
            <tbody>
            @for product in user.products
            <tr>
                <td>\${product.id}</td>
                <td>\${product.name}</td>
                <td>\${product.created_at}</td>
            </tr>
            @end
            </tbody>
		</table>
	</div>
</body>
</html>
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'templates', 'layout', 'header.html')
		content: "<nav>
	<div class='nav-wrapper'>
		<a href='javascript:window.history.back();' class='left'>
			<i class='material-icons'>arrow_back_ios_new</i>
		</a>
		<a href='/'>
			veb
		</a>
		<ul id='nav-mobile' class='right'>
			<li><a href='https://github.com/vlang/v'>github</a></li>
			<li><a href='https://vlang.io/'>website</a></li>
			<li><a href='https://github.com/sponsors/medvednikov'>support</a></li>
		</ul>
	</div>
</nav>
"
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'static', 'css', 'products.css')
		content: 'h1.title {
	font-family: Arial, Helvetica, sans-serif;
	color: #3b7bbf;
}

div.products-table {
	border: 1px solid;
	max-width: 720px;
	padding: 10px;
	margin: 10px;
}
'
	}
}
