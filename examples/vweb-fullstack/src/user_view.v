module main

import vweb
import json
import net.http

['/users'; get]
pub fn (mut app App) users() !vweb.Result {
	mut header := http.new_header()
	header.add_custom('token', app.get_cookie('token') or { '' })!

	url := 'http://localhost:8082/controller/users'
	
	mut config := http.FetchConfig{
		header: header
	}

	resp := http.fetch(http.FetchConfig{ ...config, url: url  }) or {
		app.set_status(400, '')
		return app.text('Failed to fetch data from the server. Error: ${err}')
	}
	users := json.decode([]User,resp.body)!

	return $vweb.html()
}
