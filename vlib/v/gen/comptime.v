// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import vweb.tmpl
import os

// $vweb.html()
fn (mut g Gen) vweb_html() {
	// Compile vweb html template to V code, parse that V code and embed the resulting V functions
	// that returns an html string
	mut path := g.cur_fn.name + '.html'
	println('html path=$path')
	if g.pref.is_debug {
		println('>>> compiling vweb HTML template "$path"')
	}
	if !os.exists(path) {
		// Can't find the template file in current directory,
		// try looking next to the vweb program, in case it's run with
		// v path/to/vweb_app.v
		// path = os.dir(g.scanner.file_path) + '/' + path
		// if !os.exists(path) {
		verror('vweb HTML template "$path" not found')
		// }
	}
	v_code := tmpl.compile_template(path)
	if g.pref.is_verbose {
		println('\n\n')
		println('>>> vweb template for ${path}:')
		println(v_code)
		println('>>> vweb template END')
		println('\n\n')
	}
	// is_strings_imorted := p.import_table.known_import('strings')
	// if !is_strings_imorted {
	// p.register_import('strings', 0) // used by v_code
	// }
	// p.import_table.register_used_import('strings')
	g.writeln('/////////////////// tmpl start')
	// g.statements_from_text(v_code, false, path)
	g.writeln('/////////////////// tmpl end')
	receiver := g.cur_fn.args[0]
	dot := '.' // if receiver.is_mut || receiver.ptr || receiver.typ.ends_with('*') { '->' } else { '.' }
	g.writeln('vweb__Context_html(&$receiver.name /*!*/$dot vweb, tmpl_res)')
}

fn fooo() {
}
