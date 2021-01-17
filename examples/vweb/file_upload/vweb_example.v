module main

import vweb

const (
	port = 8082
)

struct App {
	vweb.Context
}

fn main() {
	vweb.run<App>(port)
}

pub fn (mut app App) index() vweb.Result {
	return $vweb.html()
}

[post]
['/upload']
pub fn (mut app App) upload() vweb.Result {
	fdata := app.files['upfile']
	filename := fdata.filename
	filetyp := fdata.content_type

	data := vweb.RawHtml(app.form['upfile'].replace_each(['\n\r', '<br>', '\t', '	', ' ', '&nbsp;']))

	return $vweb.html()
}