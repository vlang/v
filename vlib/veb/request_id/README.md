# Request ID Middleware

This module implements request ID tracking functionality for V web applications.
Request IDs are unique identifiers assigned to each HTTP request, 
which is essential for request tracing, debugging, and maintaining distributed systems.

## Purpose

Request IDs help in:
- Tracking requests across distributed systems
- Correlating logs from different services
- Debugging and troubleshooting
- Performance monitoring
- Request chain tracing

## Usage

To enable request ID tracking in your veb app, you must embed the `RequestIdContext`
struct in your `Context` struct.

**Example:**

```v
import veb
import veb.request_id

pub struct Context {
	veb.Context
	request_id.RequestIdContext
}
```

### Basic Configuration

Here's a simple configuration example:

```v
import rand
import veb.request_id

const request_id_config = request_id.Config{
	header:    'X-Request-ID'
	generator: rand.uuid_v4
}
```

### Middleware Setup

Enable request ID tracking for all routes or specific routes using veb's middleware
system.

**Example:**

```v
import veb
import rand
import veb.request_id

pub struct Context {
	veb.Context
	request_id.RequestIdContext
}

pub struct App {
	veb.Middleware[Context]
}

const request_id_config = request_id.Config{
	header:    'X-Request-ID'
	generator: rand.uuid_v4
}

fn main() {
	mut app := &App{}
	// Register the RequestID middleware with custom configuration
	app.use(request_id.middleware[Context](request_id_config))
	veb.run[App, Context](mut app, 8080)
}
```

### Accessing the Request ID

You can access the request ID in your route handlers:

```v okfmt
import veb
import veb.request_id

fn (app &App) handler(mut ctx Context) veb.Result {
	// Get the current request ID
	request_id := ctx.request_id
	// Use the request ID for logging, etc.
	return ctx.text('Request ID: ${request_id}')
}
```

## Configuration Options

The `Config` struct provides several configuration options:

```v okfmt
pub struct Config {
pub:
	next        ?fn (ctx &veb.Context) bool
	generator   fn () string = rand.uuid_v4
	header      string       = 'X-Request-ID'
	allow_empty bool
	force       bool
}
```

### Configuration Options Explained

- `next`: Optional function to conditionally skip the middleware
- `generator`: Function to generate unique IDs (defaults to UUID v4)
- `header`: HTTP header name for the request ID (defaults to "X-Request-ID")
- `allow_empty`: Whether to allow empty request IDs
- `force`: Whether to generate a new ID even when one already exists

## Advanced Usage

### Custom ID Generator

You can provide your own ID generator function:

```v
import rand
import veb.request_id

fn custom_id_generator() string {
	return 'custom-prefix-${rand.uuid_v4()}'
}

config := request_id.Config{
	generator: custom_id_generator
}
```

### Conditional Middleware Execution

Use the `next` function to skip the middleware based on custom logic:

```v
import veb
import rand
import veb.request_id

config := request_id.Config{
	next: fn (ctx &veb.Context) bool {
		// Skip for health check endpoints
		return ctx.req.url.starts_with('/health')
	}
}
```

### Forcing New IDs

When you want to ensure a new ID is generated regardless of existing headers:

```v 
import veb.request_id

config := request_id.Config{
	force: true
}
```

## Best Practices

1. **Consistent Headers**: Use consistent header names across your services
2. **ID Propagation**: Forward request IDs to downstream services
3. **Logging Integration**: Include request IDs in your logging system
4. **ID Format**: Use a reliable ID generator (UUID v4 is recommended)

## Security Considerations

While request IDs are not security features, consider these points:
- Don't include sensitive information in request IDs
- Validate request ID format if using custom generators
- Be cautious with request ID length (recommended: 8-128 characters)

## Examples

### Basic Integration

```v
module main

import veb
import veb.request_id

pub struct Context {
	veb.Context
	request_id.RequestIdContext
}

pub struct App {
	veb.Middleware[Context]
}

@['/request-id'; get]
pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.text('Current request ID: ${ctx.request_id}')
}

fn main() {
	mut app := &App{}
	config := request_id.Config{
		header:      'X-Request-ID'
		force:       false
		allow_empty: false
	}
	app.use(request_id.middleware[Context](config))
	veb.run[App, Context](mut app, 8080)
}
```

### With Custom Generator and Conditional Execution

```v
import veb
import rand
import veb.request_id

config := request_id.Config{
	generator: fn () string {
		return 'app-${rand.uuid_v4()}'
	}
	next:      fn (ctx &veb.Context) bool {
		return ctx.req.url.starts_with('/public')
	}
}
```