module websocket

import net
import net.ssl
import log
import time
import rand

pub struct ServerState {
mut:
	ping_interval int   = 30      // interval for sending ping to clients (seconds)
	state         State = .closed // current state of connection
pub mut:
	clients map[string]&ServerClient // clients connected to this server
}

// Server represents a websocket server connection
pub struct Server {
mut:
	logger                  &log.Logger      = default_logger
	ls                      &net.TcpListener = unsafe { nil } // listener used to get incoming connection to socket
	accept_client_callbacks []AcceptClientFn      // accept client callback functions
	message_callbacks       []MessageEventHandler // new message callback functions
	close_callbacks         []CloseEventHandler   // close message callback functions
pub:
	family net.AddrFamily = .ip
	port   int  // port used as listen to incoming connections
	is_ssl bool // true if secure connection (not supported yet on server)
pub mut:
	server_state shared ServerState
}

// ServerClient represents a connected client
pub struct ServerClient {
pub:
	resource_name string // resource that the client access
	client_key    string // unique key of client
pub mut:
	server &Server = unsafe { nil }
	client &Client = unsafe { nil }
}

@[params]
pub struct ServerOpt {
pub:
	logger &log.Logger = default_logger
}

// new_server instance a new websocket server on provided port and route
pub fn new_server(family net.AddrFamily, port int, route string, opt ServerOpt) &Server {
	return &Server{
		ls:     unsafe { nil }
		family: family
		port:   port
		logger: opt.logger
	}
}

// set_ping_interval sets the interval that the server will send ping messages to clients
pub fn (mut s Server) set_ping_interval(seconds int) {
	lock s.server_state {
		s.server_state.ping_interval = seconds
	}
}

// get_ping_interval return the interval that the server will send ping messages to clients
pub fn (mut s Server) get_ping_interval() int {
	return rlock s.server_state {
		s.server_state.ping_interval
	}
}

// listen start listen and process to incoming connections from websocket clients
pub fn (mut s Server) listen() ! {
	s.logger.info('websocket server: start listen on port ${s.port}')
	s.ls = net.listen_tcp(s.family, ':${s.port}')!
	s.set_state(.open)
	spawn s.handle_ping()
	for {
		mut c := s.accept_new_client() or { continue }
		spawn s.serve_client(mut c)
	}
	s.logger.info('websocket server: end listen on port ${s.port}')
}

// Close closes server (not implemented yet)
fn (mut s Server) close() {
	// TODO: implement close when moving to net from x.net
}

// handle_ping sends ping to all clients every set interval
fn (mut s Server) handle_ping() {
	mut clients_to_remove := []string{}
	for s.get_state() == .open {
		time.sleep(s.get_ping_interval() * time.second)
		for i, _ in rlock s.server_state {
			s.server_state.clients
		} {
			mut c := rlock s.server_state {
				s.server_state.clients[i] or { continue }
			}
			if c.client.get_state() == .open {
				c.client.ping() or {
					s.logger.debug('server-> error sending ping to client')
					c.client.close(1002, 'Closing connection: ping send error') or {
						// we want to continue even if error
						continue
					}
					clients_to_remove << c.client.id
				}
				if (time.now().unix() - c.client.last_pong_ut) > s.get_ping_interval() * 2 {
					clients_to_remove << c.client.id
					c.client.close(1000, 'no pong received') or { continue }
				}
			}
		}
		// TODO: replace for with s.clients.delete_all(clients_to_remove) if (https://github.com/vlang/v/pull/6020) merges
		for client in clients_to_remove {
			lock s.server_state {
				s.server_state.clients.delete(client)
			}
		}
		clients_to_remove.clear()
	}
}

// serve_client accepts incoming connection and sets up the callbacks
fn (mut s Server) serve_client(mut c Client) ! {
	c.logger.debug('server-> Start serve client (${c.id})')
	defer {
		c.logger.debug('server-> End serve client (${c.id})')
	}
	mut handshake_response, mut server_client := s.handle_server_handshake(mut c)!
	s.attach_client(mut server_client, handshake_response)!
	c.listen() or {
		s.logger.error(err.msg())
		return err
	}
}

// handle_handshake use an existing connection to respond to the handshake for a given key
pub fn (mut s Server) handle_handshake(mut conn net.TcpConn, key string) !&ServerClient {
	mut logger := &log.Log{}
	logger.set_level(.debug)
	mut c := &Client{
		is_server:    true
		conn:         conn
		is_ssl:       false
		logger:       logger
		client_state: ClientState{
			state: .open
		}
		last_pong_ut: time.now().unix()
		id:           rand.uuid_v4()
	}
	mut server_client := &ServerClient{
		resource_name: 'GET'
		client_key:    key
		client:        unsafe { c }
		server:        unsafe { &s }
	}
	digest := create_key_challenge_response(key)!
	handshake_response := 'HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${digest}\r\n\r\n'
	s.attach_client(mut server_client, handshake_response)!
	spawn s.handle_ping()
	c.listen() or {
		s.logger.error(err.msg())
		return err
	}
	return server_client
}

fn (mut s Server) attach_client(mut server_client ServerClient, handshake_response string) ! {
	accept := s.send_connect_event(mut server_client)!
	if !accept {
		s.logger.debug('server-> client not accepted')
		server_client.client.shutdown_socket()!
		return
	}
	// the client is accepted
	server_client.client.socket_write(handshake_response.bytes())!
	lock s.server_state {
		s.server_state.clients[server_client.client.id] = unsafe { server_client }
	}
	s.setup_callbacks(mut server_client)
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
	sc.client.on_close_ref(delete_client_cb, sc)
}

fn delete_client_cb(mut c Client, code int, reason string, mut sc ServerClient) ! {
	c.logger.debug('server-> Delete client')
	lock sc.server.server_state {
		sc.server.server_state.clients.delete(sc.client.id)
	}
}

// accept_new_client creates a new client instance for client that connects to the socket
fn (mut s Server) accept_new_client() !&Client {
	mut new_conn := s.ls.accept()!
	c := &Client{
		is_server:    true
		conn:         new_conn
		ssl_conn:     ssl.new_ssl_conn()!
		logger:       s.logger
		client_state: ClientState{
			state: .open
		}
		last_pong_ut: time.now().unix()
		id:           rand.uuid_v4()
	}
	return c
}

// set_state sets current state in a thread safe way
pub fn (mut s Server) set_state(state State) {
	lock s.server_state {
		s.server_state.state = state
	}
}

// get_state return current state in a thread safe way
pub fn (s &Server) get_state() State {
	return rlock s.server_state {
		s.server_state.state
	}
}

// free manages manual free of memory for Server instance
pub fn (mut s Server) free() {
	lock s.server_state {
		unsafe {
			s.server_state.clients.free()
		}
	}

	unsafe {
		s.accept_client_callbacks.free()
		s.message_callbacks.free()
		s.close_callbacks.free()
	}
}
