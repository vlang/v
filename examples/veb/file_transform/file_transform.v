module main

import veb

const port = 8082

pub struct Context {
	veb.Context
}

pub struct App {
}

fn main() {
	mut app := &App{}
	veb.run[App, Context](mut app, port)
}

pub fn (mut app App) index() veb.Result {
	return $veb.html()
}

@['/upload'; post]
pub fn (mut app App) upload(mut ctx Context) veb.Result {
	fdata := ctx.files['upfile']

	data_rows := fdata[0].data.split('\n')

	mut output_data := ''

	for elem in data_rows {
		delim_row := elem.split('\t')
		output_data += '${delim_row[0]}\t${delim_row[1]}\t${delim_row[0].int() + delim_row[1].int()}\n'
	}

	output_data = output_data.all_before_last('\n')

	println(output_data)

	ctx.set_header(.content_disposition, 'attachment; filename=results.txt')
	ctx.send_response_to_client('application/octet-stream', output_data)

	return $veb.html()
}
