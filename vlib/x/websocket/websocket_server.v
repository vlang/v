// The module websocket implements the websocket server capabilities
module websocket

import net
import x.openssl
import log
import sync
import time
import rand

// Server holds state of websocket server connection
pub struct Server {
mut:
	clients                 map[string]&ServerClient // Clients connected to this server
	logger                  &log.Log // Logger used to log
	ls                      net.TcpListener // TCpLister used to get incoming connection to socket
	accept_client_callbacks []AcceptClientFn // Accept client callback functions
	message_callbacks       []MessageEventHandler // New message callback functions
	close_callbacks         []CloseEventHandler // Close message callback functions
pub:
	port                    int // Port used as listen to incoming connections
	is_ssl                  bool // True if secure connection (not supported yet on server)
pub mut:
	ping_interval           int = 30
	// Interval for automatic sending ping to connected clients in seconds
	state                   State // Current state of connection
}

// ServerClient has state of connected clients
struct ServerClient {
pub:
	resource_name string // The resource that the client access
	client_key    string // Unique key of client
pub mut:
	server        &Server // The server instance
	client        &Client // The client instance
}

// new_server instance new websocket server on port and route
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

// listen, start listen to incoming connections
pub fn (mut s Server) listen() ? {
	s.logger.info('websocket server: start listen on port $s.port')
	s.ls = net.listen_tcp(s.port) ?
	s.set_state(.open)
	go s.handle_ping()
	for {
		mut c := s.accept_new_client() or {
			continue
		}
		go s.serve_client(mut c)
	}
	s.logger.info('websocket server: end listen on port $s.port')
}

// Close server (not implemented)
fn (mut s Server) close() {
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
					c.client.close(1000, 'no pong received') or {
						continue
					}
				}
			}
		}
		// TODO replace for with s.clients.delete_all(clients_to_remove) if (https://github.com/vlang/v/pull/6020) merges
		for client in clients_to_remove {
			lock  {
				s.clients.delete(client)
			}
		}
		clients_to_remove.clear()
	}
}

// serve_client accepts incoming connection and setup the websocket handshake
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
	// The client is accepted
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
	// Set standard close so we can remove client if closed
	sc.client.on_close_ref(fn (mut c Client, code int, reason string, mut sc ServerClient) ? {
		c.logger.debug('server-> Delete client')
		lock  {
			sc.server.clients.delete(sc.client.id)
		}
	}, sc)
}

// accept_new_client creates a new client instance for client connects to socket
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

// free, manual free memory of Server instance
pub fn (mut s Server) free() {
	unsafe {
		s.clients.free()
		s.accept_client_callbacks.free()
		s.message_callbacks.free()
		s.close_callbacks.free()
	}
}
