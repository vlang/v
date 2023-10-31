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
	fdata := app.files['upfile']

	data_rows := fdata[0].data.split('\n')

	mut output_data := ''

	for elem in data_rows {
		delim_row := elem.split('\t')
		output_data += '${delim_row[0]}\t${delim_row[1]}\t${delim_row[0].int() + delim_row[1].int()}\n'
	}

	output_data = output_data.all_before_last('\n')

	println(output_data)

	app.add_header('Content-Disposition', 'attachment; filename=results.txt')
	app.send_response_to_client('application/octet-stream', output_data)

	return $vweb.html()
}
