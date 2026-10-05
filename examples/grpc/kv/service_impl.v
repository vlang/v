// The service half of the example: one in-memory key-value store implementing
// net.grpc's GrpcService interface.
//
// The dispatch has the shape gRPC needs for every RPC flavour: a list of
// request messages in, a list of response messages out. So `found=false` plus
// one reply is unary, one reply per server-streamed message is server-streaming,
// and a run of replies from many requests is client-streaming — the codec and
// the transport stay out of it.
//
// A failing handler returns a `grpc.StatusError` as its *error*, not as a
// payload. `GrpcServer` turns that into the grpc-status trailer, which is what
// lets the client see a typed code instead of a transport error.
//
// This file is hand-written and is not generated. Only the method paths
// (`kv_method_*`, from `service.v`) and the codec (`codec.v`) come from files
// that `v pbgen` generates from kv.proto. `Service` deliberately does not
// implement the generated `KV` interface: its handlers need a
// `grpc.ServerContext` to set headers and trailers (e.g. `x-kv-scanned`), and
// that transport-free interface has no parameter for one.
module kv

import net.grpc

// Service is an in-memory KV store serving the KV service of kv.proto.
pub struct Service {
mut:
	store map[string][]u8
}

// new_service returns an empty store.
pub fn new_service() Service {
	return Service{
		store: map[string][]u8{}
	}
}

// grpc_call dispatches one RPC. A `found` of false means the path belongs to a
// different service, letting several services share one server.
pub fn (mut s Service) grpc_call(path string, reqs [][]u8, mut ctx grpc.ServerContext) !([][]u8, bool) {
	match path {
		kv_method_get {
			return [s.get(decode_get_request(reqs[0])!, mut ctx)!], true
		}
		kv_method_put {
			return [s.put(decode_put_request(reqs[0])!, mut ctx)!], true
		}
		kv_method_scan {
			return s.scan(decode_get_request(reqs[0])!, mut ctx)!, true
		}
		kv_method_put_many {
			return s.put_many(reqs, mut ctx)!, true
		}
		else {
			return [][]u8{}, false
		}
	}
}

// get answers one GetRequest. A missing key is a successful response with
// `found = false`, not an error: "no such key" is a normal answer, whereas a
// malformed request is not.
fn (mut s Service) get(req GetRequest, mut ctx grpc.ServerContext) ![]u8 {
	ctx.set_header('x-kv-lookup', 'get')
	value := s.store[req.key] or { return GetResponse{}.encode()! }
	return GetResponse{
		value: value
		found: true
	}.encode()!
}

// put answers one PutRequest, reporting whether it overwrote anything.
fn (mut s Service) put(req PutRequest, mut ctx grpc.ServerContext) ![]u8 {
	if req.key.len == 0 {
		return grpc.StatusError{
			status: grpc.Status{
				code:    .invalid_argument
				message: 'key must not be empty'
			}
		}
	}
	replaced := req.key in s.store
	s.store[req.key] = req.value
	ctx.set_trailer('x-kv-replaced', if replaced { '1' } else { '0' })
	return PutResponse{
		replaced: replaced
	}.encode()!
}

// scan answers one GetRequest with every stored entry whose key starts with
// `req.key`, which is what server-streaming looks like from the handler side:
// several reply messages, then net.grpc closes the stream with grpc-status: 0.
// An empty prefix therefore scans everything.
fn (mut s Service) scan(req GetRequest, mut ctx grpc.ServerContext) ![][]u8 {
	mut replies := [][]u8{}
	mut keys := s.store.keys()
	keys.sort()
	for key in keys {
		if !key.starts_with(req.key) {
			continue
		}
		replies << GetResponse{
			value: s.store[key] or { []u8{} }
			found: true
		}.encode()!
	}
	ctx.set_trailer('x-kv-scanned', '${replies.len}')
	return replies
}

// put_many answers a whole client-streamed request: many PutRequests came in, so
// it returns a single PutManyResponse counting them.
fn (mut s Service) put_many(reqs [][]u8, mut ctx grpc.ServerContext) ![][]u8 {
	mut written := 0
	for raw in reqs {
		req := decode_put_request(raw)!
		if req.key.len == 0 {
			return grpc.StatusError{
				status: grpc.Status{
					code:    .invalid_argument
					message: 'key must not be empty'
				}
			}
		}
		s.store[req.key] = req.value
		written++
	}
	ctx.set_trailer('x-kv-written', '${written}')
	return [PutManyResponse{
		written: written
	}.encode()!]
}
