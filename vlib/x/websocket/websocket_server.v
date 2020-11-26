// The module websocket implements the websocket server capabilities
module websocket

import net
import x.openssl
import log
import sync
import time
import rand

pub struct Server {
mut:
	clients                 map[string]&ServerClient
	logger                  &log.Log
	ls                      net.TcpListener
	accept_client_callbacks []AcceptClientFn
	message_callbacks       []MessageEventHandler
	close_callbacks         []CloseEventHandler
pub:
	port                    int
	is_ssl                  bool
pub mut:
	ping_interval           int = 30 // in seconds
	state                   State
}

struct ServerClient {
pub:
	resource_name string
	client_key    string
pub mut:
	server        &Server
	client        &Client
}

pub fn new_server(port int, route string) &Server {
	return &Server{
		port: port
		logger: &log.Log{
			level: .info
		}
		state: .closed
	}
}

pub fn (mut s Server) set_ping_interval(seconds int) {
	s.ping_interval = seconds
}

pub fn (mut s Server) listen() ? {
	s.logger.info('websocket server: start listen on port $s.port')
	s.ls = net.listen_tcp(s.port)?
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

fn (mut s Server) close() {
}

// Todo: make thread safe
fn (mut s Server) handle_ping() {
	mut clients_to_remove := []string{}
	for s.state == .open {
		time.sleep(s.ping_interval)
		for _, cli in s.clients {
			mut c := cli
			if c.client.state == .open {
				c.client.ping() or {
					s.logger.debug('server-> error sending ping to client')
					// todo fix better close message, search the standard
					c.client.close(1002, 'Clossing connection: ping send error') or {
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

fn (mut s Server) serve_client(mut c Client) ? {
	c.logger.debug('server-> Start serve client ($c.id)')
	defer {
		c.logger.debug('server-> End serve client ($c.id)')
	}
	mut handshake_response, mut server_client := s.handle_server_handshake(mut c)?
	accept := s.send_connect_event(mut server_client)?
	if !accept {
		s.logger.debug('server-> client not accepted')
		c.shutdown_socket()?
		return
	}
	// The client is accepted
	c.socket_write(handshake_response.bytes())?
	lock  {
		s.clients[server_client.client.id] = server_client
	}
	s.setup_callbacks(mut server_client)
	c.listen() or {
		s.logger.error(err)
		return error(err)
	}
}

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

fn (mut s Server) accept_new_client() ?&Client {
	mut new_conn := s.ls.accept()?
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
[inline]
fn (mut s Server) set_state(state State) {
	lock  {
		s.state = state
	}
}

pub fn (mut s Server) free() {
	unsafe {
		s.clients.free()
		// s.logger.free()
		// s.ls.free()
		s.accept_client_callbacks.free()
		s.message_callbacks.free()
		s.close_callbacks.free()
	}
}
