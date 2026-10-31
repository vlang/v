module main

// This example demonstrates how to use `v watch` for veb apps, that use sqlite .db files.
//
// Note 1: while developing services, it is useful to also add the `--keep` option of `v watch`,
// which will restart the app right away, even when it exits on its own.
//
// Note 2: veb has a live reload mode. Compiled with `-d veb_livereload`, its pages update in place
// whenever `v watch` restarts the app, keeping scroll position and form input; meanwhile
// `v watch` builds the new version while the old one keeps serving.
//
// The above means, that to get the most optimal prototyping experience for veb apps, use:
// `v watch -d veb_livereload --keep run .`
// (put `-d veb_livereload` after `watch`, see https://github.com/vlang/v/issues/30199)
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
	println('`v watch -d veb_livereload --keep run examples/vwatch/web_server/`')
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
