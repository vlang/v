# Fasthttp Example

A simple HTTP server example using the `fasthttp` module from `vlib/fasthttp`.

## Features

- Handles GET and POST requests
- Routes requests to different controllers based on HTTP method and path
- Returns appropriate HTTP responses with status codes and content

## Building

```sh
./v examples/fasthttp
```

## Running

```sh
./examples/fasthttp/fasthttp
```

The server will listen on `http://localhost:3000`

## Testing

### Home endpoint

```sh
curl http://localhost:3000/
```

### Get user by ID

```sh
curl http://localhost:3000/user/123
```

### Create user

```sh
curl -X POST http://localhost:3000/user
```

### 404 response

```sh
curl http://localhost:3000/notfound
```

## File Structure

- `main.c.v` - Entry point and request router
- `controllers.v` - Request handlers for different routes
- `v.mod` - Module metadata

## Architecture

The example demonstrates:

1. **Request Routing**: The `handle_request()` function routes incoming HTTP requests based on
   method and path
2. **Response Handling**: Controllers return HTTP responses with proper headers and status codes
3. **Content Type**: All responses are returned as `[]u8` (byte arrays)

The fasthttp module handles:

- Low-level socket management
- Request parsing
- Connection handling
- Non-blocking I/O with epoll (Linux) or kqueue (macOS)
