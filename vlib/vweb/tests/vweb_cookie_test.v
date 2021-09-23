import vweb
import net.http

const (
	port = 12380
)

struct App {
	vweb.Context
}

fn (mut app App) index() vweb.Result {
	app.set_cookie(vweb.Cookie{
		name: 'darkmode'
		value: '0'
		path: '/' // <------ new
		domain: '127.0.0.1' // <------ new
	})

	if cookie := app.get_cookie('darkmode') {
		println(cookie)
	}

	return app.html('<h1>Hello world</h1>')
}

fn test_cookie() {
	go vweb.run(&App{}, port)
	resp := http.get('http://127.0.0.1:$port/') or { panic(err) }
	darkmode := resp.cookies['darkmode'] or { panic('cookie not set') }
	value := darkmode[0].ascii_str().int()
	mut path := ''
	mut domain := ''
	path_index := darkmode.index('path=') or { panic("path doesn't exist") } + 5
	domain_index := darkmode.index('domain=') or { panic("domain doesn't exist") } + 7
	for ch in darkmode[path_index..] {
		if ch == `;` {
			break
		}
		path += ch.ascii_str()
	}
	for ch in darkmode[domain_index..] {
		if ch == `;` {
			break
		}
		domain += ch.ascii_str()
	}
	assert value == 0
	assert path == '/'
	assert domain == '127.0.0.1'
	println(resp.cookies)
}
