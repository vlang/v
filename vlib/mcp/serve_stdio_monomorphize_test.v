module mcp

import io
import strings

// ClosedStdin is an input that is already at end of file.
struct ClosedStdin {}

fn (mut r ClosedStdin) read(mut _buf []u8) !int {
	return io.Eof{}
}

// test_serve_stdio_transport_compiles_after_monomorphize serves an empty input through
// the transport serve_stdio uses. The regression is a compile-time crash in the scoped
// monomorphize pass, reached from this function's callees.
fn test_serve_stdio_transport_compiles_after_monomorphize() {
	mut server := new_server(
		name:    'graphify'
		version: '0.0.1'
	)
	mut reader := ClosedStdin{}
	mut writer := strings.new_builder(0)
	server.serve_stdio_transport(mut reader, mut writer, stdio_session_id, .stdio) or {
		assert false, 'serve_stdio_transport failed: ${err}'
		return
	}
	assert writer.len == 0
}
