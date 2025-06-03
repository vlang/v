<img src="./logo.png" alt="vanilla_http_server Logo" width="100">

# vanilla_http_server

- **Fast**: Multi-threaded, non-blocking I/O, lock-free, copy-free, epoll, SO_REUSEPORT.
- **Thread Affinity**: Work in Progress (W.I.P.).
- **Modular**: Compatible with any HTTP parser.
- **Memory Safety**: No race conditions.
- **No Magic**: Transparent and straightforward.
- **E2E Testing**: Allows end-to-end testing and scripting without running the server.
  Simply pass the raw request to `handle_request()`.
- **SSE Friendly**: Server-Sent Events support.
- **Graceful Shutdown**: Work in Progress (W.I.P.).

## Installation

### From Root Directory

1. Create the required directories:

```bash
mkdir -p ~/.vmodules/enghitalo/vanilla
```

2. Copy the `vanilla_http_server` directory to the target location:

```bash
cp -r ./ ~/.vmodules/enghitalo/vanilla
```

3. Run the example:

```bash
v -prod crun examples/simple
```

This sets up the module in your `~/.vmodules` directory for use.

### From Repository

Install directly from the repository:

```bash
v install https://github.com/enghitalo/vanilla_http_server
```

## Benchmarking

Run the following commands to benchmark the server:

1. Test with `curl`:

```bash
curl -v http://localhost:3001
```

2. Test with `wrk`:

```bash
wrk -H 'Connection: "keep-alive"' --connection 512 --threads 16 --duration 60s http://localhost:3001
```

Example output:

```plaintext
Running 1m test @ http://localhost:3001
  16 threads and 512 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
   Latency     1.25ms    1.46ms  35.70ms   84.67%
   Req/Sec    32.08k     2.47k   57.85k    71.47%
  30662010 requests in 1.00m, 2.68GB read
Requests/sec: 510197.97
Transfer/sec:     45.74MB
```
