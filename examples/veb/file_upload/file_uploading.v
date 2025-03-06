module main

import veb

const port = 8082

struct App {
}

struct Context {
	veb.Context
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
	mut files := []veb.RawHtml{}
	for d in fdata {
		files << d.data.replace_each(['\n', '<br>', '\n\r', '<br>', '\t', '	', ' ', '&nbsp;'])
	}
	return $veb.html()
}

@['/submit'; post]
pub fn (mut app App) submit() veb.Result {
	dump(ctx.form)
	form_data := ctx.form.clone()
	return $veb.html()
}
