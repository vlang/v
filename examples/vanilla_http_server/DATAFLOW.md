# Vanilla HTTP Server - Data Flow

This document describes the complete data flow through the high-performance HTTP server, from connection establishment to response delivery.

## System Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                        Main Thread                           │
│  - Creates 16 worker threads                                 │
│  - Each worker gets its own listen socket (SO_REUSEPORT)     │
│  - Each worker gets its own epoll instance                   │
│  - Waits for workers (blocking, no CPU waste)                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
        ┌──────────────────────────────────────────┐
        │          Worker Threads (x16)            │
        │  Each runs process_events() in loop      │
        └──────────────────────────────────────────┘
                              │
                              ▼
```

## Connection Flow

### 1. Initial Setup (per worker)

```
create_server_socket(port)
├── socket(AF_INET, SOCK_STREAM, 0)
├── set_blocking(fd, false)           // Non-blocking listen socket
├── setsockopt(SO_REUSEADDR)          // Allow immediate rebind
├── setsockopt(SO_REUSEPORT)          // Multiple sockets on same port
├── bind(0.0.0.0:3001)
└── listen(backlog=4096)

epoll_create1(0)                      // Create per-worker epoll
└── add_fd_to_epoll(listen_fd, EPOLLIN | EPOLLET | EPOLLEXCLUSIVE)
```

**Key Points:**

- Each worker has its own listening socket bound to the same port
- `SO_REUSEPORT` enables kernel-level load balancing
- `EPOLLEXCLUSIVE` prevents thundering herd on accept

### 2. Accept New Connection

```
epoll_wait() returns EPOLLIN on listen_fd
└── handle_accept_loop(epoll_fd, listen_fd)
    └── Loop until EAGAIN:
        ├── accept4(listen_fd, NULL, NULL, SOCK_NONBLOCK)
        │   └── Returns client_fd (already non-blocking)
        ├── setsockopt(client_fd, TCP_NODELAY, 1)
        │   └── Disable Nagle's algorithm for low latency
        └── add_fd_to_epoll(epoll_fd, client_fd, EPOLLIN | EPOLLET)
            └── Register client for read events (edge-triggered)
```

**Data Path:**

```
Client connects → Kernel queues on one listen socket (SO_REUSEPORT)
              → One worker woken (EPOLLEXCLUSIVE)
              → accept4() returns non-blocking client_fd
              → TCP_NODELAY enabled
              → Client fd added to worker's epoll
```

### 3. Request Processing

```
epoll_wait() returns EPOLLIN on client_fd
└── Process readable event:
    ├── recv(client_fd, buffer, 140-1, 0)
    │   └── Read HTTP request into reused buffer
    │
    ├── decode_http_request(buffer)
    │   ├── parse_request_line()
    │   │   ├── Extract method (GET/POST/etc)
    │   │   ├── Extract path (/user/123)
    │   │   └── Extract HTTP version (HTTP/1.1)
    │   └── Return HttpRequest{buffer, method, path, version}
    │
    ├── server.request_handler(decoded_request)
    │   ├── Route to controller based on method + path
    │   │   ├── GET /         → home_controller()
    │   │   ├── GET /user/:id → get_user_controller(id)
    │   │   └── POST /user    → create_user_controller()
    │   └── Return response buffer []u8
    │
    └── send(client_fd, response, len, MSG_NOSIGNAL | MSG_DONTWAIT)
        └── Non-blocking send with no SIGPIPE
```

**Data Structure Flow:**

```
Raw bytes [140]u8
    ↓ (push_many)
[]u8 (heap-allocated, exact size)
    ↓ (decode_http_request)
HttpRequest {
    buffer: []u8
    method: Slice{start, len}
    path: Slice{start, len}
    version: Slice{start, len}
}
    ↓ (request_handler)
Response []u8 (e.g., "HTTP/1.1 200 OK\r\n...")
    ↓ (send)
TCP packets to client
```

### 4. Keep-Alive Behavior

```
After send():
├── Connection remains open (Connection: keep-alive)
├── Client fd stays in epoll
└── Next request from same client:
    └── epoll_wait() triggers EPOLLIN again
        └── Back to step 3
```

**Connection Lifecycle:**

```
accept4() → add to epoll → [recv/send loop] → close on:
                                                  ├── recv() = 0 (FIN)
                                                  ├── recv() < 0 && errno != EAGAIN
                                                  └── EPOLLHUP | EPOLLERR
```

### 5. Connection Closure

```
Trigger: recv() == 0 or EPOLLHUP/EPOLLERR
└── handle_client_closure(epoll_fd, client_fd)
    ├── remove_fd_from_epoll(epoll_fd, client_fd)
    │   └── epoll_ctl(EPOLL_CTL_DEL)
    └── close_socket(client_fd)
        └── close(client_fd) with EINTR retry
```

**Closure Scenarios:**

1. **Clean shutdown**: Client sends FIN → `recv()` returns 0
2. **Error state**: Socket error → `EPOLLERR` event
3. **Client disconnect**: Connection reset → `EPOLLHUP` event
4. **Decode failure**: Invalid HTTP → send 400, then close
5. **Handler error**: Internal error → send 400, then close

## Memory Management

### Buffer Reuse Strategy

```
process_events() {
    mut events := [4096]C.epoll_event{}      // Stack, reused per loop
    mut request_buffer := [140]u8{}          // Stack, reused per loop

    for {  // Event loop
        num_events := epoll_wait(&events[0], 4096, -1)

        for i in 0..num_events {
            // Per-request heap allocation only when needed
            mut readed_request_buffer := []u8{cap: bytes_read}
            readed_request_buffer.push_many(&request_buffer[0], bytes_read)

            // HttpRequest references the buffer (no copy)
            decoded := decode_http_request(readed_request_buffer)

            // Response may be static const or dynamically built
            response := request_handler(decoded)
        }
    }
}
```

**Allocation Profile:**

- **Stack**: Event array (32KB), request buffer (140 bytes)
- **Heap**: Per-request buffer allocation (exact size), response buffers
- **Zero-copy**: HttpRequest uses Slices pointing into buffer

## Performance Optimizations

### 1. Edge-Triggered Mode (EPOLLET)

```
Level-triggered (default):
- epoll_wait() returns every time fd is readable
- Wastes CPU on repeated notifications

Edge-triggered (EPOLLET):
- epoll_wait() returns only on state change
- Must drain recv() until EAGAIN
- Higher throughput, lower CPU
```

### 2. EPOLLEXCLUSIVE

```
Without EPOLLEXCLUSIVE:
- New connection wakes ALL workers
- Only one can accept(), others waste CPU (thundering herd)

With EPOLLEXCLUSIVE:
- Kernel wakes only ONE worker
- Eliminates wasted wakeups
- ~10% throughput improvement
```

### 3. SO_REUSEPORT

```
Single listen socket:
- All workers contend on accept() lock
- Kernel bottleneck

Per-worker socket (SO_REUSEPORT):
- Kernel load-balances incoming connections
- No shared lock contention
- Near-linear scaling with cores
```

### 4. TCP_NODELAY

```
Nagle's algorithm (default):
- Buffers small writes for efficiency
- Adds latency (up to 200ms)

TCP_NODELAY:
- Sends immediately
- Critical for request-response patterns
- Reduces P99 latency
```

### 5. MSG_NOSIGNAL | MSG_DONTWAIT

```
Default send():
- Raises SIGPIPE if peer closed
- Blocks if send buffer full

MSG_NOSIGNAL | MSG_DONTWAIT:
- Returns EPIPE instead of signal
- Returns EAGAIN instead of blocking
- Allows graceful error handling
```

## Concurrency Model

```
┌─────────────┐   ┌─────────────┐   ┌─────────────┐
│  Worker 1   │   │  Worker 2   │   │  Worker N   │
│             │   │             │   │             │
│ listen_fd=3 │   │ listen_fd=4 │   │ listen_fd=N │
│ epoll_fd=5  │   │ epoll_fd=6  │   │ epoll_fd=M  │
│             │   │             │   │             │
│ ┌─────────┐ │   │ ┌─────────┐ │   │ ┌─────────┐ │
│ │ Client  │ │   │ │ Client  │ │   │ │ Client  │ │
│ │ Pool    │ │   │ │ Pool    │ │   │ │ Pool    │ │
│ └─────────┘ │   │ └─────────┘ │   │ └─────────┘ │
└─────────────┘   └─────────────┘   └─────────────┘
       ▲                ▲                  ▲
       └────────────────┴──────────────────┘
            Kernel distributes via SO_REUSEPORT
```

**No Shared State:**

- Each worker is independent
- No locks or atomics in hot path
- Perfect CPU cache locality

## Error Handling

```
Request Processing Errors:
├── decode_http_request() fails
│   └── Send 400 Bad Request + close
├── request_handler() fails
│   └── Send 400 Bad Request + close
└── send() returns -1
    ├── EAGAIN → Ignore (would need send buffer)
    ├── EPIPE → Close connection
    └── Other → Log and close

Connection Errors:
├── accept4() fails
│   ├── EAGAIN → Break (no more pending)
│   └── Other → Log, continue loop
├── recv() == 0
│   └── Clean FIN, close gracefully
└── EPOLLHUP | EPOLLERR
    └── Force close
```

## Metrics & Observability

The server is optimized for throughput over observability. Removed metrics:

- No per-request timing
- No closure reason tracking
- No error counters

For debugging, use:

```sh
# System-level metrics
ss -s                          # Socket statistics
netstat -an | grep 3001        # Connection states

# Application tracing
strace -p <pid> -e epoll_wait,accept4,recv,send

# Performance profiling
perf record -F 99 -p <pid>
perf report
```

## Benchmark Results

```
Configuration: 16 workers, 512 concurrent connections, 10 seconds
Hardware: Multi-core x86_64

Results:
- Requests/sec: 510,480
- Latency P50:  1.29ms
- Latency P99:  ~5ms (estimated from stddev)
- Throughput:   30.18 MB/sec
- CPU:          ~95% (16 cores saturated)
```

**Bottlenecks:**

1. Client receive buffer (wrk limitation)
2. Context switching overhead
3. System call cost (recv/send)

**Future Optimizations:**

- **io_uring**: Zero-copy I/O with submission/completion queues, eliminating syscall overhead
- **Batched sends**: Use `writev()` or `sendmsg()` with scatter-gather to send response in one syscall
- **Response caching**: Pre-serialize common responses at startup, bypass routing/handler for cache hits
- **CPU affinity**: Pin worker threads to specific cores to improve cache locality
- **DPDK bypass**: Bypass kernel network stack entirely for maximum throughput (userspace TCP/IP)
- **HTTP/2 multiplexing**: Share single connection for multiple requests, reduce connection overhead
- **JIT compilation**: V's `-prod` with PGO (profile-guided optimization) for hot paths
- **Memory pool**: Pre-allocate buffers in arena to eliminate allocation overhead
- **Lock-free queues**: If cross-worker communication needed, use MPSC queues instead of shared epoll
