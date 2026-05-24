module main

// This example demonstrates how to use `v watch` for veb apps, that use sqlite .db files.
//
// Note 1: while developing services, it is useful to also add the `--keep` option of `v watch`,
// which will restart the app right away, even when it exits on its own.
//
// Note 2: veb supports a special live reload mode, where it will make the browser to check for server
// restarts, and it will trigger a refresh of the current page, right after that is detected.
//
// The above means, that to get the most optimal prototyping experience for veb apps, use:
// `v -d veb_livereload watch --only-watch=*.v,*.html,*.css,*.js --keep run .`
import os
import veb
import db.sqlite

fn mydb() !sqlite.DB {
	return sqlite.connect(os.resource_abs_path('app.db'))
}

struct State {
mut:
	counter int
}

pub struct Context {
	veb.Context
}

struct App {
	veb.StaticHandler
mut:
	state shared State
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	mut c := 0
	lock app.state {
		app.state.counter++
		c = app.state.counter
	}
	visits := app.update_db() or { 0 }
	return ctx.html('<!doctype html><html><body>
	<br/>Current request counter, after the server restart: ${c}.
	<br/>Total stored visits: ${visits}
	</body></html>')
}

fn (mut app App) update_db() !int {
	mut db := mydb()!
	db.exec('INSERT INTO visits (created_at) VALUES ("")')!
	visits := db.q_int('SELECT count(*) FROM visits')!
	db.close()!
	return visits
}

fn main() {
	println('App demonstrating the use of `veb` & `db.sqlite` together.')
	println('For best prototyping experience, run with:')
	println('`v -d veb_livereload watch --keep run examples/vwatch/web_server/`')
	println('')
	mut db := mydb()!
	db.exec('CREATE TABLE visits (id integer primary key AUTOINCREMENT, created_at timestamp default current_timestamp);')!
	db.exec('CREATE TRIGGER INSERT_visits AFTER INSERT ON visits BEGIN
	  UPDATE visits SET created_at = datetime("now", "localtime") WHERE rowid = new.rowid ;
		END')!
	db.close()!
	mut app := &App{}
	veb.run[App, Context](mut app, 19123)
}
