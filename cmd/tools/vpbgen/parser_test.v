module main

import os

// test_dir is where the .proto fixtures and their expected .v output live.
const test_dir = os.join_path(os.dir(vexe), 'cmd', 'tools', 'vpbgen', 'tests')

// vexe is the compiler path, used to locate the fixtures. `VEXE` is set by the
// launcher; `@VEXE` is the compile-time fallback.
const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

fn fixture(name string) string {
	return os.join_path(test_dir, name)
}

fn test_parse_kv_proto() {
	// This is the schema the gRPC example under `examples/grpc` is written
	// against, so parsing it is the check that the generator understands the
	// shape a real schema takes.
	f := parse_file(fixture('kv.proto'))!
	assert f.syntax == 'proto3'
	assert f.package == 'kv'
	assert f.package_parts == ['kv']
	assert f.messages.len == 5
	assert f.services.len == 1
}

fn test_parse_message_fields() {
	f := parse_file(fixture('kv.proto'))!
	req := f.messages[0]
	assert req.name == 'GetRequest'
	assert req.fields.len == 1
	fld := req.fields[0]
	assert fld.name == 'key'
	assert fld.number == 1
	assert fld.label == .singular
	assert fld.type_name == 'string'
	assert fld.kind == .text

	resp := f.messages[1]
	assert resp.name == 'GetResponse'
	assert resp.fields.len == 2
	assert resp.fields[0].type_name == 'bytes'
	assert resp.fields[0].kind == .bytes
	assert resp.fields[1].name == 'found'
	assert resp.fields[1].number == 2
	assert resp.fields[1].type_name == 'bool'
	assert resp.fields[1].kind == .scalar
}

fn test_parse_negative_and_scalar_kinds() {
	f := parse_file(fixture('kv.proto'))!
	counts := f.messages[4]
	assert counts.fields[0].name == 'written'
	assert counts.fields[0].number == 1
	assert counts.fields[0].type_name == 'int32'
	assert counts.fields[0].kind == .scalar
}

fn test_parse_service_and_rpcs() {
	f := parse_file(fixture('kv.proto'))!
	svc := f.services[0]
	assert svc.name == 'KV'
	assert svc.rpcs.len == 4

	// unary
	assert svc.rpcs[0].name == 'Get'
	assert svc.rpcs[0].request_type == 'GetRequest'
	assert svc.rpcs[0].response_type == 'GetResponse'
	assert !svc.rpcs[0].client_stream
	assert !svc.rpcs[0].server_stream

	// server streaming
	assert svc.rpcs[2].name == 'Scan'
	assert svc.rpcs[2].server_stream
	assert !svc.rpcs[2].client_stream

	// client streaming
	assert svc.rpcs[3].name == 'PutMany'
	assert svc.rpcs[3].client_stream
	assert !svc.rpcs[3].server_stream
}

fn test_rpc_comments_are_captured() {
	f := parse_file(fixture('kv.proto'))!
	svc := f.services[0]
	assert svc.rpcs[0].comments == ['unary: one request, one response']
	assert svc.rpcs[2].comments == ['server streaming: one request, many responses']
}

fn test_classify_type() {
	assert classify_type('string') == .text
	assert classify_type('bytes') == .bytes
	for name in [
		'double',
		'float',
		'int32',
		'int64',
		'uint32',
		'uint64',
		'sint32',
		'sint64',
		'fixed32',
		'sfixed32',
		'fixed64',
		'sfixed64',
		'bool',
	] {
		assert classify_type(name) == .scalar, '${name} should be a scalar'
	}
	// anything else is a message or an enum, which only resolution can tell
	// apart
	assert classify_type('Status') == .message
	assert classify_type('google.rpc.Status') == .message
}
