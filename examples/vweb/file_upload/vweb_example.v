module main

import vweb

struct App {}

fn main() {
	vweb.run<App>(port: 8082)
}

pub fn (mut app App) index(mut c vweb.Context) vweb.Result {
	return $vweb.html()
}

[post]
['/upload']
pub fn (mut app App) upload(mut c vweb.Context) vweb.Result {
	fdata := c.files['upfile']

	mut files := []vweb.RawHtml{}

	for d in fdata {
		files << d.data.replace_each(['\n', '<br>', '\n\r', '<br>' '\t', '	', ' ', '&nbsp;'])
	}

	return $vweb.html()
}
