# Redis Client for V

This module provides a Redis client implementation in V that supports the 
Redis Serialization Protocol (RESP) with a type-safe interface for common Redis commands.

## Features

- **Type-Safe Commands**: Auto-detects value types at compile time
- **Pipeline Support**: Group commands for batch execution
- **Connection Pooling**: Efficient resource management
- **RESP Protocol Support**: Full Redis Serialization Protocol implementation
- **Memory Efficient**: Pre-allocated buffers for minimal allocations

## Quick Start

```v
module main

import db.redis

fn main() {
	// Connect to Redis
	mut db := redis.connect(redis.Config{
		host: 'localhost'
		port: 6379
	})!

	// Set and get values
	db.set('name', 'Alice')!
	name := db.get[string]('name')!
	println('Name: ${name}') // Output: Name: Alice

	// Integer operations
	db.set('counter', 42)!
	db.incr('counter')!
	counter := db.get[int]('counter')!
	println('Counter: ${counter}') // Output: Counter: 43

	// Clean up
	db.close()!
}
```

## Supported Commands

### Key Operations

```v ignore
// Set value
db.set('key', 'value')!
db.set('number', 42)!
db.set('binary', []u8{len: 4, init: 0})!

// Get value
str_value := db.get[string]('key')!
int_value := db.get[int]('number')!
bin_value := db.get[[]u8]('binary')!

// Delete key
db.del('key')!

// Set expiration
db.expire('key', 60)!  // 60 seconds
```

### Hash Operations

```v ignore
// Set hash fields
db.hset('user:1', {
    'name': 'Bob',
    'age': 30,
})!

// Get single field
name := db.hget[string]('user:1', 'name')!

// Get all fields
user_data := db.hgetall[string]('user:1')!
println(user_data)  // Output: {'name': 'Bob', 'age': '30'}
```

### Pipeline Operations

```v ignore
// Start pipeline
db.pipeline_start()

// Queue commands
db.incr('counter')
db.set('name', 'Charlie')
db.get[string]('name')

// Execute and get responses
responses := db.pipeline_execute()!
for resp in responses {
    println(resp)
}
```

### Custom Commands

```v ignore
// Run raw commands
resp := db.cmd('SET', 'custom', 'value')!
result := db.cmd('GET', 'custom')!

// Complex commands
db.cmd('HSET', 'user:2', 'field1', 'value1', 'field2', '42')!
```

## Error Handling

All functions return `!` types and will return detailed errors for:

- Connection issues
- Protocol violations
- Type mismatches
- Redis error responses
- Timeout conditions

```v ignore
result := db.get[string]('nonexistent') or {
    println('Key not found')
    return
}
```

## Connection Management

```v ignore
config := redis.Config{
    host: 'redis.server'
    port: 6379
    version: 2  // RESP2 protocol
}

mut db := redis.connect(config)!
defer {
    db.close() or { eprintln('Error closing connection: $err') }
}
```

## Performance Tips

1. **Reuse Connections**: Maintain connections instead of creating new ones
2. **Use Pipelines**: Batch commands for high-throughput operations
3. **Prefer Integers**: Use numeric types for counters and metrics
4. **Specify Types**: Always specify return types for get operations