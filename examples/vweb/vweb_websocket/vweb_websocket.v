module main

import log
import net.http
import net.websocket
import term
import vweb

const http_port = 8080

struct App {
	vweb.Context
mut:
	wss &websocket.Server @[vweb_global]
}

fn slog(message string) {
	eprintln(term.colorize(term.bright_yellow, message))
}

fn clog(message string) {
	eprintln(term.colorize(term.cyan, message))
}

fn wlog(message string) {
	eprintln(term.colorize(term.bright_blue, message))
}

fn main() {
	mut app := new_app() or { panic(err) }
	vweb.run(app, http_port)
}

fn new_app() !&App {
	mut app := &App{
		wss: new_websocker_server()!
	}
	app.handle_static('assets', true)
	return app
}

fn new_websocker_server() !&websocket.Server {
	mut wss := &websocket.Server{
		logger: &log.Log{
			level: .debug
		}
	}
	wss.on_connect(fn (mut server_client websocket.ServerClient) !bool {
		slog('ws.on_connect, server_client.client_key: ${server_client.client_key}')
		return true
	})!
	wss.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ! {
		slog('s.on_message msg.opcode: ${msg.opcode} | msg.payload: ${msg.payload}')
		ws.write(msg.payload, msg.opcode) or {
			eprintln('ws.write err: ${err}')
			return err
		}
	})
	wss.on_close(fn (mut ws websocket.Client, code int, reason string) ! {
		slog('s.on_close code: ${code}, reason: ${reason}')
	})
	slog('Websocket Server initialized')
	return wss
}

pub fn (mut app App) index() vweb.Result {
	return $vweb.html()
}

pub fn (mut app App) ws() !vweb.Result {
	key := app.req.header.get(http.CommonHeader.sec_websocket_key)!
	app.wss.handle_handshake(mut app.conn, key) or {
		wlog('handle_handshake error: ${err.msg()}')
		return err
	}
	return app.text('')
}
