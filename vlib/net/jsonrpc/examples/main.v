module main

import net
import sync
import jsonrpc
import log

// ---- CRUD domain ----
struct KvItem {
	key   string
	value string
}

struct KvKey {
	key string
}

// ---- Handler ----
struct KvStore {
mut:
	mu    &sync.RwMutex = sync.new_rwmutex()
	store map[string]string
}

fn (mut s KvStore) create(key string, value string) bool {
	s.mu.@lock()
	defer { s.mu.unlock() }
	if key in s.store {
		return false
	}
	s.store[key] = value
	return true
}

fn (mut s KvStore) get(key string) ?string {
	s.mu.@rlock()
	defer { s.mu.runlock() }
	if value := s.store[key] {
		return value
	}
	return none
}

fn (mut s KvStore) update(key string, value string) bool {
	s.mu.@lock()
	defer { s.mu.unlock() }
	if key in s.store {
		s.store[key] = value
		return true
	}
	return false
}

fn (mut s KvStore) delete(key string) bool {
	s.mu.@lock()
	defer { s.mu.unlock() }
	if key in s.store {
		s.store.delete(key)
		return true
	}
	return false
}

fn (s KvStore) dump() map[string]string {
	return s.store
}

@[heap]
struct KvHandler {
mut:
	store KvStore
}

fn (mut h KvHandler) handle_create(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter) {
	p := req.decode_params[KvItem]() or {
		wr.write_error(jsonrpc.invalid_params)
		return
	}
	if p.key.len == 0 {
		wr.write_error(jsonrpc.invalid_params)
		return
	}
	log.warn("params=${p}")
	if !h.store.create(p.key, p.value) {
		wr.write_error(jsonrpc.ResponseError{ // custom app-level error code
			code: -32010
			message: 'Key already exists'
			data: p.key
		})
		return
	}

	wr.write({ 'ok': true })
}

fn (mut h KvHandler) handle_get(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter) {
	p := req.decode_params[KvKey]() or {
		wr.write_error(jsonrpc.invalid_params)
		return
	}

	value := h.store.get(p.key) or {
		wr.write_error(jsonrpc.ResponseError{
			code: -32004
			message: 'Not found'
			data: p.key
		})
		return
	}
	
	wr.write(KvItem{ key: p.key, value: value })
}

fn (mut h KvHandler) handle_update(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter) {
	p := req.decode_params[KvItem]() or {
		wr.write_error(jsonrpc.invalid_params)
		return
	}
	
	if !h.store.update(p.key, p.value) {
		wr.write_error(jsonrpc.ResponseError{
			code: -32004
			message: 'Not found'
			data: p.key
		})
		return
	}
	
	wr.write({ 'ok': true })
}

fn (mut h KvHandler) handle_delete(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter) {
	p := req.decode_params[KvKey]() or {
		wr.write_error(jsonrpc.invalid_params)
		return
	}

	if !h.store.delete(p.key) {
		wr.write_error(jsonrpc.ResponseError{
			code: -32004
			message: 'Not found'
			data: p.key
		})
		return
	}

	wr.write({ 'ok': true })
}

fn (mut h KvHandler) handle_list(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter) {
	mut items := []KvItem{}
	for k, v in h.store.dump() {
		items << KvItem{ key: k, value: v }
	}
	items.sort(a.key < b.key)
	wr.write(items)
}

// ---- Per-connection server loop ----
// The jsonrpc.Server.start() reads from stream and writes to same stream.
fn handle_conn(mut conn net.TcpConn, h jsonrpc.Handler) {
	defer { conn.close() or {} }

	mut log_inter := jsonrpc.LoggingInterceptor{}
	mut inters := jsonrpc.Interceptors{
		event: [log_inter.on_event]
		encoded_request: [log_inter.on_encoded_request]
		request: [log_inter.on_request]
		response: [log_inter.on_response]
		encoded_response: [log_inter.on_encoded_response]
	}

	mut srv := jsonrpc.new_server(jsonrpc.ServerConfig{
		stream: conn
		handler: h
		interceptors: inters
	})

	jsonrpc.dispatch_event(inters.event, "start", "server started")
	srv.start()
}

fn main() {
	mut s := KvStore{}
	mut h := KvHandler{store: s}
	mut r := jsonrpc.Router{}
	r.register('kv.create', h.handle_create)
	r.register('kv.get', h.handle_get)
	r.register('kv.update', h.handle_update)
	r.register('kv.delete', h.handle_delete)
	r.register('kv.list', h.handle_list)

	addr := '127.0.0.1:42228'
	mut l := net.listen_tcp(.ip, addr)!
	println('TCP JSON-RPC server on ${addr} (Content-Length framing)')

	for {
		mut c := l.accept()!
		println("Accepted")
		go handle_conn(mut c, r.handle_jsonrpc)
	}
}
