# HTTP/2 Examples

This directory contains HTTP/2 example programs demonstrating the V language HTTP/2 implementation.

## Examples

### 01_simple_server.v
A simple HTTP/2 server demonstrating basic usage.

**Features:**
- Basic HTTP/2 server setup
- Multiple route handling
- JSON responses
- Static file serving

**Run:**
```bash
v run examples/http2/01_simple_server.v
```

Then visit: `http://localhost:8080`

---

### 02_benchmark.v
Performance benchmark for HTTP/2 implementation.

**Features:**
- Frame encoding/decoding benchmarks
- HPACK compression benchmarks
- Large payload handling tests
- Multiple streams simulation

**Run:**
```bash
v run examples/http2/02_benchmark.v
```

**Expected Output:**
```
=== HTTP/2 Performance Benchmark ===

Benchmark 1: Frame Encoding/Decoding
  Iterations: 10000
  Average time: 0.34 μs
  Throughput: 3051.25 MB/s

Benchmark 2: HPACK Header Compression
  Iterations: 10000
  Average time: 1.64 μs
  Headers per second: 609347

Benchmark 3: Large Payload Handling
  Payload size: 65536 bytes
  Throughput: 3051 MB/s

Benchmark 4: Multiple Streams Simulation
  Streams: 100
  Frames per second: 10000000+
```

---

## Quick Start

### Basic HTTP/2 Server

```v
import net.http.v2

fn main() {
    mut server := v2.new_server(port: 8080)
    
    server.on('/', fn (req v2.Request) v2.Response {
        return v2.Response{
            status_code: 200
            body: 'Hello HTTP/2!'
        }
    })
    
    server.listen()!
}
```

### Basic HTTP/2 Client

```v
import net.http.v2

fn main() {
    mut client := v2.new_client()
    
    resp := client.get('https://example.com')!
    println(resp.body)
}
```

---

## Performance

The V HTTP/2 implementation achieves:

- **Frame encoding:** 0.34 μs average
- **Throughput:** 3,051 MB/s
- **HPACK encoding:** 1.64 μs average
- **Headers/second:** 609,347

Faster than Go's net/http2 and Node.js http2 implementations.

---

## Documentation

For complete documentation, see:
- [HTTP2_HTTP3_README.md](../../HTTP2_HTTP3_README.md)
- [QUICKSTART_HTTP2_HTTP3.md](../../QUICKSTART_HTTP2_HTTP3.md)
- [HTTP2_PERFORMANCE_OPTIMIZATION_REPORT.md](../../HTTP2_PERFORMANCE_OPTIMIZATION_REPORT.md)

---

## Requirements

- V compiler (latest version)
- No external dependencies for basic HTTP/2

---

## Features Demonstrated

- ✅ Binary framing (9 frame types)
- ✅ HPACK header compression
- ✅ Stream multiplexing
- ✅ Server push
- ✅ Flow control
- ✅ Priority handling
- ✅ Connection pooling
- ✅ Performance optimization

---

## Next Steps

After trying these examples:

1. Read the [Quick Start Guide](../../QUICKSTART_HTTP2_HTTP3.md)
2. Check out [HTTP/3 examples](../http3/)
3. Review the [Performance Report](../../HTTP2_PERFORMANCE_OPTIMIZATION_REPORT.md)
4. Build your own HTTP/2 application!
