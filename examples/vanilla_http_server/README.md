# Vanilla HTTP Server

A high-performance HTTP server written in V using epoll-based I/O multiplexing.

## Performance

Achieves **~510k requests/sec** on a multi-core system with the following characteristics:

```
Running 10s test @ http://localhost:3001
  16 threads and 512 connections
  Latency     1.29ms    1.72ms  29.25ms   85.00%
  Req/Sec    32.20k     7.62k   57.52k    73.19%
  5144559 requests in 10.08s, 304.19MB read
  Requests/sec: 510480.43
  Transfer/sec:     30.18MB
```

## Architecture

### Core Design

- **Per-worker listen sockets**: Each thread has its own listening socket using SO_REUSEPORT to
  avoid contention
- **Distributed accept**: Accepts are handled in worker threads via EPOLLEXCLUSIVE to prevent
  thundering herd
- **Edge-triggered epoll**: EPOLLET mode for efficient event processing
- **Keep-alive connections**: Reuse connections across multiple requests
- **Non-blocking I/O**: All sockets are non-blocking with accept4(SOCK_NONBLOCK)
- **Minimal allocations**: Reused request buffer across all events

### Socket Options

- `SO_REUSEADDR` & `SO_REUSEPORT`: Enable multi-socket binding on the same port
- `TCP_NODELAY`: Disable Nagle's algorithm for lower latency
- `MSG_NOSIGNAL`: Avoid SIGPIPE when writing to closed sockets
- `MSG_DONTWAIT`: Non-blocking send operations

## Usage

### Build

```sh
v run examples/vanilla_http_server/src
```

Production build:

```sh
v -prod crun examples/vanilla_http_server/src
```

### Benchmark

```sh
# Standard benchmark (16 threads, 512 connections)
wrk -t16 -c512 -d10s http://127.0.0.1:3001/

# High concurrency benchmark
wrk -t24 -c1024 -d10s http://127.0.0.1:3001/

# HTTP/1.1 requests
curl -v http://localhost:3001/
```

## System Tuning

For optimal performance, increase system limits:

```sh
# Increase file descriptor limit
ulimit -n 65536

# Increase socket backlog (Linux)
sudo sysctl -w net.core.somaxconn=4096
sudo sysctl -w net.ipv4.tcp_max_syn_backlog=4096
```

## Code Structure

- `src/server.c.v`: Core server implementation with epoll event loop
- `src/request_parser.v`: HTTP request parsing (lightweight)
- `src/controllers.v`: Request handlers
- `src/main.c.v`: Entry point

## Request Handler

The server uses a fast-path architecture:

1. Accept incoming connections in per-worker threads
2. Parse HTTP request with `decode_http_request()`
3. Route to handler via `request_handler()`
4. Send response and keep connection alive
5. Close on EOF or errors

## Performance Optimization Techniques

1. **Reduced allocations**: Stack-allocated event array and request buffer
2. **Batched processing**: 4096 max events per epoll_wait call
3. **No metrics overhead**: Removed closure reason tracking in hot path
4. **Fast socket setup**: TCP_NODELAY enabled immediately on accept
5. **Non-blocking sends**: MSG_DONTWAIT prevents blocking on full buffers
6. **Proper thread coordination**: Main thread waits for workers instead of spinning

## Limitations

- Single-threaded request handling per worker (no request pipelining within worker)
- Static HTTP response bodies (for benchmarking)
- No HTTP/2 or multiplexing
- IPv4 only
