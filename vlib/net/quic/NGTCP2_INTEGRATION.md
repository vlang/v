# ngtcp2 Integration Guide for V Language

## Overview

This document describes the integration of ngtcp2 (a C QUIC library) with V's HTTP/3 implementation.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    V HTTP/3 Application                      │
│                  (vlib/net/http/v3/client.v)                 │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   V QUIC Wrapper Layer                       │
│                (vlib/net/quic/quic_ngtcp2.v)                 │
│  - Connection management                                     │
│  - Stream operations                                         │
│  - Handshake handling                                        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   ngtcp2 C Bindings                          │
│                 (vlib/net/quic/ngtcp2.c.v)                   │
│  - FFI declarations                                          │
│  - Type definitions                                          │
│  - Wrapper functions                                         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    ngtcp2 C Library                          │
│  - QUIC protocol implementation                              │
│  - Packet handling                                           │
│  - Crypto operations                                         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    OpenSSL (TLS 1.3)                         │
│  - Encryption/Decryption                                     │
│  - Key derivation                                            │
│  - Certificate validation                                    │
└─────────────────────────────────────────────────────────────┘
```

## File Structure

```
vlib/net/quic/
├── quic.v                      # Public API entry point
├── ngtcp2.c.v                  # C bindings for ngtcp2
├── quic_ngtcp2.v               # V wrapper implementation
├── ngtcp2_test.v               # Tests for ngtcp2 bindings
├── QUIC_LIBRARY_EVALUATION.md  # Library comparison
└── NGTCP2_INTEGRATION.md       # This file
```

## Installation

### macOS

```bash
brew install ngtcp2
```

### Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install libngtcp2-dev libssl-dev
```

### Build from Source

```bash
git clone https://github.com/ngtcp2/ngtcp2
cd ngtcp2
autoreconf -i
./configure --with-openssl
make
sudo make install
```

### Verify Installation

```bash
# Check if library is installed
pkg-config --modversion libngtcp2

# Check if headers are available
ls /usr/local/include/ngtcp2/
```

## API Reference

### Core Types

#### Connection
```v
pub struct Connection {
pub mut:
    remote_addr    string
    conn_id        []u8
    streams        map[u64]&Stream
    next_stream_id u64
    closed         bool
    ngtcp2_conn    voidptr  // ngtcp2 connection handle
    udp_socket     net.UdpConn
    handshake_done bool
}
```

#### ConnectionConfig
```v
pub struct ConnectionConfig {
pub:
    remote_addr string
    alpn        []string = ['h3']
    enable_0rtt bool
    max_stream_data_bidi_local  u64 = 1048576   // 1MB
    max_stream_data_bidi_remote u64 = 1048576   // 1MB
    max_stream_data_uni         u64 = 1048576   // 1MB
    max_data                    u64 = 10485760  // 10MB
    max_streams_bidi            u64 = 100
    max_streams_uni             u64 = 100
    max_idle_timeout            u64 = 30000     // 30s
}
```

### Functions

#### new_connection
Creates a new QUIC connection.

```v
pub fn new_connection(config ConnectionConfig) !Connection
```

**Example:**
```v
import net.quic

config := quic.ConnectionConfig{
    remote_addr: 'example.com:443'
    alpn: ['h3']
    enable_0rtt: true
}

mut conn := quic.new_connection(config) or {
    eprintln('Failed to create connection: ${err}')
    return
}

// Perform handshake
conn.perform_handshake() or {
    eprintln('Handshake failed: ${err}')
    return
}

// Use connection...
conn.close()
```

#### open_stream
Opens a new bidirectional stream.

```v
pub fn (mut c Connection) open_stream() !u64
```

**Example:**
```v
stream_id := conn.open_stream() or {
    eprintln('Failed to open stream: ${err}')
    return
}
println('Opened stream ${stream_id}')
```

#### send
Sends data on a stream.

```v
pub fn (mut c Connection) send(stream_id u64, data []u8) !
```

**Example:**
```v
data := 'Hello, HTTP/3!'.bytes()
conn.send(stream_id, data) or {
    eprintln('Failed to send data: ${err}')
    return
}
```

#### recv
Receives data from a stream.

```v
pub fn (mut c Connection) recv(stream_id u64) ![]u8
```

**Example:**
```v
response := conn.recv(stream_id) or {
    eprintln('Failed to receive data: ${err}')
    return
}
println('Received ${response.len} bytes')
```

#### close_stream
Closes a stream.

```v
pub fn (mut c Connection) close_stream(stream_id u64) !
```

#### close
Closes the connection.

```v
pub fn (mut c Connection) close()
```

## ngtcp2 C Bindings

### Key Functions Bound

| C Function | V Wrapper | Description |
|------------|-----------|-------------|
| `ngtcp2_conn_client_new` | `conn_client_new` | Create client connection |
| `ngtcp2_conn_del` | `conn_del` | Delete connection |
| `ngtcp2_conn_read_pkt` | `conn_read_pkt` | Process received packet |
| `ngtcp2_conn_write_pkt` | `conn_write_pkt` | Generate packet to send |
| `ngtcp2_conn_writev_stream` | `conn_writev_stream` | Write stream data |
| `ngtcp2_conn_open_bidi_stream` | `conn_open_bidi_stream` | Open bidirectional stream |
| `ngtcp2_conn_shutdown_stream` | `conn_shutdown_stream` | Close stream |
| `ngtcp2_settings_default` | `settings_default` | Initialize settings |
| `ngtcp2_strerror` | `strerror` | Get error string |

### Error Codes

```v
pub const (
    ngtcp2_err_invalid_argument = -201
    ngtcp2_err_nobuf            = -203
    ngtcp2_err_proto            = -205
    ngtcp2_err_invalid_state    = -206
    ngtcp2_err_stream_not_found = -222
    ngtcp2_err_internal         = -238
    // ... more error codes
)
```

## Usage with HTTP/3

The QUIC connection is automatically used by the HTTP/3 client:

```v
import net.http

// Automatic HTTP/3 with QUIC
resp := http.get('https://example.com')!
println('Status: ${resp.status_code}')
println('Body: ${resp.body}')
```

The version negotiation will:
1. Try HTTP/3 (QUIC)
2. Fall back to HTTP/2 if QUIC fails
3. Fall back to HTTP/1.1 if HTTP/2 fails

## Implementation Status

### ✅ Completed

- [x] ngtcp2 C bindings (`ngtcp2.c.v`)
- [x] Connection management (`quic_ngtcp2.v`)
- [x] Stream operations (open, send, recv, close)
- [x] Settings and transport parameters
- [x] Error handling
- [x] UDP socket integration
- [x] Basic handshake flow

### ⚠️ Partial

- [ ] TLS 1.3 integration (needs crypto callbacks)
- [ ] Complete handshake implementation
- [ ] Connection migration
- [ ] 0-RTT support
- [ ] Session resumption

### ❌ Not Implemented

- [ ] Crypto callbacks (encrypt/decrypt)
- [ ] Certificate validation
- [ ] ALPN negotiation
- [ ] Connection pooling
- [ ] Congestion control tuning
- [ ] Flow control management
- [ ] Loss detection callbacks
- [ ] Path validation

## Next Steps

### Phase 1: Complete Handshake (Priority: High)

1. **Implement crypto callbacks**
   - `client_initial` - Generate initial secrets
   - `recv_crypto_data` - Process TLS messages
   - `encrypt` / `decrypt` - Packet protection
   - `hp_mask` - Header protection

2. **TLS 1.3 integration**
   - Use OpenSSL for TLS handshake
   - Implement key derivation
   - Handle certificate validation

3. **Test with real servers**
   - google.com (supports HTTP/3)
   - cloudflare.com (supports HTTP/3)
   - quic.tech (QUIC test server)

### Phase 2: Advanced Features (Priority: Medium)

1. **0-RTT support**
   - Session ticket handling
   - Early data transmission
   - Replay protection

2. **Connection migration**
   - PATH_CHALLENGE / PATH_RESPONSE
   - Address validation
   - NAT rebinding

3. **Performance optimization**
   - Connection pooling
   - Packet pacing
   - GSO (Generic Segmentation Offload)

### Phase 3: Production Readiness (Priority: Low)

1. **Comprehensive testing**
   - Unit tests for all functions
   - Integration tests with real servers
   - Stress testing
   - Fuzzing

2. **Documentation**
   - API documentation
   - Usage examples
   - Performance tuning guide
   - Troubleshooting guide

3. **Monitoring and debugging**
   - Connection statistics
   - Packet loss tracking
   - RTT measurements
   - Debug logging

## Troubleshooting

### ngtcp2 not found

**Error:**
```
error: cannot find -lngtcp2
```

**Solution:**
```bash
# macOS
brew install ngtcp2

# Ubuntu
sudo apt-get install libngtcp2-dev

# Check installation
pkg-config --libs libngtcp2
```

### OpenSSL version mismatch

**Error:**
```
error: ngtcp2 requires OpenSSL 1.1.1 or later
```

**Solution:**
```bash
# macOS
brew install openssl@1.1

# Ubuntu
sudo apt-get install libssl-dev

# Check version
openssl version
```

### Connection timeout

**Error:**
```
error: handshake timeout
```

**Possible causes:**
1. Server doesn't support HTTP/3
2. UDP port 443 blocked by firewall
3. QUIC not enabled on server
4. Network doesn't support UDP

**Solution:**
- Check if server supports HTTP/3: `curl -I --http3 https://example.com`
- Check firewall rules
- Try with a known HTTP/3 server (e.g., google.com)

### Stream errors

**Error:**
```
error: stream not found
```

**Solution:**
- Ensure stream is opened before sending data
- Check stream ID (client uses odd IDs: 0, 4, 8, ...)
- Verify handshake is complete

## Performance Considerations

### Buffer Sizes

```v
// Recommended buffer sizes
send_buf: []u8{len: 65536}  // 64KB
recv_buf: []u8{len: 65536}  // 64KB
```

### Connection Limits

```v
// Recommended limits for typical use
max_streams_bidi: 100
max_data: 10485760  // 10MB
max_idle_timeout: 30000  // 30 seconds
```

### UDP Socket Options

```v
// Enable for better performance
- SO_REUSEADDR
- SO_RCVBUF (increase receive buffer)
- SO_SNDBUF (increase send buffer)
```

## Security Considerations

1. **Certificate Validation**
   - Always validate server certificates
   - Check certificate chain
   - Verify hostname

2. **ALPN**
   - Always specify ALPN protocol ('h3')
   - Reject unexpected protocols

3. **Connection Limits**
   - Set reasonable max_data limits
   - Limit number of streams
   - Set idle timeout

4. **0-RTT**
   - Be aware of replay attacks
   - Don't send sensitive data in 0-RTT
   - Implement replay protection

## References

- [ngtcp2 Documentation](https://nghttp2.org/ngtcp2/)
- [QUIC RFC 9000](https://www.rfc-editor.org/rfc/rfc9000.html)
- [HTTP/3 RFC 9114](https://www.rfc-editor.org/rfc/rfc9114.html)
- [TLS 1.3 RFC 8446](https://www.rfc-editor.org/rfc/rfc8446.html)
- [V C Interop](https://github.com/vlang/v/blob/master/doc/docs.md#calling-c-functions-from-v)

## Contributing

To contribute to the ngtcp2 integration:

1. Read this document thoroughly
2. Check the implementation status above
3. Pick a task from "Next Steps"
4. Write tests for your implementation
5. Submit a PR with clear documentation

## License

The ngtcp2 integration follows V's MIT license. ngtcp2 itself is also MIT licensed.

---

**Last Updated**: January 23, 2026  
**Status**: Phase 1 - Basic bindings complete, handshake in progress  
**Maintainer**: V Core Team
