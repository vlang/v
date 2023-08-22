module main

// This example demonstrates how to use `v watch` for vweb apps, that use sqlite .db files.
//
// Note 1: while developing services, it is useful to also add the `--keep` option of `v watch`,
// which will restart the app right away, even when it exits on its own.
//
// Note 2: vweb supports a special live reload mode, where it will make the browser to check for server
// restarts, and it will trigger a refresh of the current page, right after that is detected.
//
// The above means, that to get the most optimal prototyping experience for vweb apps, use:
// `v -d vweb_livereload watch --only-watch=*.v,*.html,*.css,*.js --keep run .`
import os
import vweb
import db.sqlite

fn mydb() !sqlite.DB {
	return sqlite.connect(os.resource_abs_path('app.db'))
}

struct State {
mut:
	counter int
}

struct App {
	vweb.Context
mut:
	state shared State
}

pub fn (mut app App) index() vweb.Result {
	mut c := 0
	lock app.state {
		app.state.counter++
		c = app.state.counter
	}
	visits := app.update_db() or { 0 }
	return app.html('<!doctype html><html><body>
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
	println('App demonstrating the use of `vweb` & `db.sqlite` together.')
	println('For best prototyping experience, run with:')
	println('`v -d vweb_livereload watch --keep run examples/vwatch/web_server/`')
	println('')
	mut db := mydb()!
	db.exec('CREATE TABLE visits (id integer primary key AUTOINCREMENT, created_at timestamp default current_timestamp);')!
	db.exec('CREATE TRIGGER INSERT_visits AFTER INSERT ON visits BEGIN
	  UPDATE visits SET created_at = datetime("now", "localtime") WHERE rowid = new.rowid ;
		END')!
	db.close()!
	vweb.run(&App{}, 19123)
}
