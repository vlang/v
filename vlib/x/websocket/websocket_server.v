module websocket

import net
import x.openssl
import log
import sync
import time
import rand

// Server represents a websocket server connection
pub struct Server {
mut:
	clients                 map[string]&ServerClient // clients connected to this server
	logger                  &log.Log // logger used to log
	ls                      net.TcpListener // listener used to get incoming connection to socket
	accept_client_callbacks []AcceptClientFn // accept client callback functions
	message_callbacks       []MessageEventHandler // new message callback functions
	close_callbacks         []CloseEventHandler // close message callback functions
pub:
	port                    int // port used as listen to incoming connections
	is_ssl                  bool // true if secure connection (not supported yet on server)
pub mut:
	ping_interval           int = 30
	// interval for sending ping to clients (seconds)
	state                   State // current state of connection
}

// ServerClient represents a connected client
struct ServerClient {
pub:
	resource_name string // resource that the client access
	client_key    string // unique key of client
pub mut:
	server        &Server
	client        &Client
}

// new_server instance a new websocket server on provided port and route
pub fn new_server(port int, route string) &Server {
	return &Server{
		port: port
		logger: &log.Log{
			level: .info
		}
		state: .closed
	}
}

// set_ping_interval sets the interval that the server will send ping messages to clients
pub fn (mut s Server) set_ping_interval(seconds int) {
	s.ping_interval = seconds
}

// listen start listen and process to incoming connections from websocket clients
pub fn (mut s Server) listen() ? {
	s.logger.info('websocket server: start listen on port $s.port')
	s.ls = net.listen_tcp(s.port) ?
	s.set_state(.open)
	go s.handle_ping()
	for {
		mut c := s.accept_new_client() or { continue }
		go s.serve_client(mut c)
	}
	s.logger.info('websocket server: end listen on port $s.port')
}

// Close closes server (not implemented yet)
fn (mut s Server) close() {
	// TODO: implement close when moving to net from x.net
}

// handle_ping sends ping to all clients every set interval
fn (mut s Server) handle_ping() {
	mut clients_to_remove := []string{}
	for s.state == .open {
		time.sleep(s.ping_interval)
		for _, cli in s.clients {
			mut c := cli
			if c.client.state == .open {
				c.client.ping() or {
					s.logger.debug('server-> error sending ping to client')
					c.client.close(1002, 'Closing connection: ping send error') or {
						// we want to continue even if error
						continue
					}
					clients_to_remove << c.client.id
				}
				if (time.now().unix - c.client.last_pong_ut) > s.ping_interval * 2 {
					clients_to_remove << c.client.id
					c.client.close(1000, 'no pong received') or { continue }
				}
			}
		}
		// TODO: replace for with s.clients.delete_all(clients_to_remove) if (https://github.com/vlang/v/pull/6020) merges
		for client in clients_to_remove {
			lock  {
				s.clients.delete(client)
			}
		}
		clients_to_remove.clear()
	}
}

// serve_client accepts incoming connection and sets up the callbacks
fn (mut s Server) serve_client(mut c Client) ? {
	c.logger.debug('server-> Start serve client ($c.id)')
	defer {
		c.logger.debug('server-> End serve client ($c.id)')
	}
	mut handshake_response, mut server_client := s.handle_server_handshake(mut c) ?
	accept := s.send_connect_event(mut server_client) ?
	if !accept {
		s.logger.debug('server-> client not accepted')
		c.shutdown_socket() ?
		return
	}
	// the client is accepted
	c.socket_write(handshake_response.bytes()) ?
	lock  {
		s.clients[server_client.client.id] = server_client
	}
	s.setup_callbacks(mut server_client)
	c.listen() or {
		s.logger.error(err)
		return error(err)
	}
}

// setup_callbacks initialize all callback functions
fn (mut s Server) setup_callbacks(mut sc ServerClient) {
	if s.message_callbacks.len > 0 {
		for cb in s.message_callbacks {
			if cb.is_ref {
				sc.client.on_message_ref(cb.handler2, cb.ref)
			} else {
				sc.client.on_message(cb.handler)
			}
		}
	}
	if s.close_callbacks.len > 0 {
		for cb in s.close_callbacks {
			if cb.is_ref {
				sc.client.on_close_ref(cb.handler2, cb.ref)
			} else {
				sc.client.on_close(cb.handler)
			}
		}
	}
	// set standard close so we can remove client if closed
	sc.client.on_close_ref(fn (mut c Client, code int, reason string, mut sc ServerClient) ? {
		c.logger.debug('server-> Delete client')
		lock  {
			sc.server.clients.delete(sc.client.id)
		}
	}, sc)
}

// accept_new_client creates a new client instance for client that connects to the socket
fn (mut s Server) accept_new_client() ?&Client {
	mut new_conn := s.ls.accept() ?
	c := &Client{
		is_server: true
		conn: new_conn
		ssl_conn: openssl.new_ssl_conn()
		logger: s.logger
		state: .open
		last_pong_ut: time.now().unix
		id: rand.uuid_v4()
	}
	return c
}

// set_state sets current state in a thread safe way
fn (mut s Server) set_state(state State) {
	lock  {
		s.state = state
	}
}

// free manages manual free of memory for Server instance
pub fn (mut s Server) free() {
	unsafe {
		s.clients.free()
		s.accept_client_callbacks.free()
		s.message_callbacks.free()
		s.close_callbacks.free()
	}
}
