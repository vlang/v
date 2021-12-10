module main

import vweb

const port = 8082

struct App {
	vweb.Context
}

fn main() {
	vweb.run(&App{}, port)
}

pub fn (mut app App) index() vweb.Result {
	return $vweb.html()
}

['/upload'; post]
pub fn (mut app App) upload() vweb.Result {
	dump(app.form)
	dump(app.files)
	fdata := app.files['upfile']
	mut files := []vweb.RawHtml{}
	for d in fdata {
		files << d.data.replace_each(['\n', '<br>', '\n\r', '<br>', '\t', '	', ' ', '&nbsp;'])
	}

	return $vweb.html()
}

['/submit'; post]
pub fn (mut app App) submit() vweb.Result {
	dump(app.form)
	form_data := app.form.clone()
	return $vweb.html()
}
