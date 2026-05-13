module main

import net.http
import veb

const port = 8082

struct App {
}

struct Context {
	veb.Context
}

fn uploaded_file_contents(fdata []http.FileData) []veb.RawHtml {
	mut files := []veb.RawHtml{}
	for data in fdata {
		files << data.data.replace_each(['\n', '<br>', '\n\r', '<br>', '\t', '	', ' ', '&nbsp;'])
	}
	return files
}

fn main() {
	mut app := &App{}
	veb.run[App, Context](mut app, port)
}

pub fn (mut app App) index() veb.Result {
	return $veb.html()
}

@['/upload'; post]
pub fn (mut app App) upload() veb.Result {
	dump(ctx.form)
	dump(ctx.files)
	fdata := ctx.files['upfile']
	files := uploaded_file_contents(fdata)
	return $veb.html()
}

@['/submit'; post]
pub fn (mut app App) submit() veb.Result {
	dump(ctx.form)
	form_data := ctx.form.clone()
	return $veb.html()
}
