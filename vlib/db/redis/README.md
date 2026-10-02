# Redis Client for V

This module provides a Redis client implementation in V that supports the 
Redis Serialization Protocol (RESP) versions 2 (RESP2) and 3 (RESP3) with
a type-safe interface for common Redis commands.

## Features

- **Type-Safe Commands**: String, key, and hash operations with compile-time value types
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
	// Uncomment passwod line if authentication is needed
	mut db := redis.connect(redis.Config{
		// Available Config options (none of which need to be specified if you want the defauilts):
		// host: 'localhost' - default
		// password: 'your_password' - no default, you need to supply password if your redis server
		//                             is set up to need one
		// port: 6379 - default
		// tls: false - default, set to true for ssl connection
	})!
	println('Server supports RESP${db.version} protocol')

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

### String and Key Commands

String commands include `mget`, `mset`, `msetnx`, `setnx`, `setex`, `psetex`, `getdel`,
`getex`, `incrby`, `decrby`, `incrbyfloat`, `append`, `strlen`, `getrange`, and `setrange`.
The value-taking commands accept strings, integers, or `[]u8`, just like `set`.

```v ignore
db.mset({'first': 'Alice', 'second': ''})!
values := db.mget[string]('first', 'missing', 'second')!
// values is []?string: entries keep request order, and missing keys are none.
// An existing empty value is an option containing '', distinct from none.

// Set a value only when the key does not exist.
created := db.setnx('lock', 'owner')!

// Set a value with an expiration, or change expiration while reading it.
db.setex('cache', 60, 'value')!
value := db.getex[string]('cache', mode: .px, value: 5000)!
db.getex[string]('cache', mode: .persist)!

// Read and delete atomically.
previous := db.getdel[string]('cache')!

// Counters and byte ranges.
db.incrby('counter', 10)!
db.incrbyfloat('score', 0.5)!
db.append('first', ' Smith')!
part := db.getrange[string]('first', 0, 4)!
```

`getdel` and `getex` require Redis 6.2 or later. Like `get`, they return an error for a
missing key. `mget[T]` and `hmget[T]` return nullable entries for strings, integers, or `[]u8`.
`GetExOptions` selects one `mode`: `.none`, `.ex`, `.px`, `.exat`, `.pxat`, or `.persist`.
The four expiration modes require a positive `value`; `.none` and `.persist` use zero.

Key commands include `exists`, `ttl`, `pttl`, `pexpire`, `expireat`, `pexpireat`, `persist`,
`keys`, `scan`, `key_type`, `rename`, and `unlink`, in addition to `del` and `expire`.
`key_type` sends Redis's `TYPE` command. `exists` returns the number of existing requested keys.
`ttl` and `pttl` preserve Redis's negative results: `-1` means no expiration, and `-2` means
that the key does not exist. `unlink` deletes one or more keys with asynchronous reclamation.

```v ignore
count := db.exists('first', 'second', 'missing')!
remaining_ms := db.pttl('first')!
db.pexpire('first', 5000)!
db.persist('first')!
kind := db.key_type('first')!
db.rename('first', 'renamed')!
db.unlink('renamed', 'second')!
```

`scan` and `hscan` accept `ScanOptions` with optional `match` and `count` fields. Both return
`(cursor, entries)`, where the cursor is a string. Start with `'0'` and continue until the
returned cursor is `'0'`. A page can be empty before iteration finishes; `count` is a hint.
`hscan` entries alternate field names and values. Redis may return duplicate entries while
iterating, and callers should handle them when needed.
Omitting `match` scans all entries; `match: ''` matches an empty key or field name.

```v ignore
mut cursor := '0'
for {
    next, entries := db.scan(cursor, match: 'user:*', count: 100)!
    for key in entries {
        println(key)
    }
    cursor = next
    if cursor == '0' {
        break
    }
}
```

### Hash Operations
```v ignore
// Set hash fields
db.hset('user:1', {
    'name': 'Bob',
    'age': '30',
})!

// Get single field
name := db.hget[string]('user:1', 'name')!

// Get all fields
user_data := db.hgetall[string]('user:1')!
println(user_data)  // Output: {'name': 'Bob', 'age': '30'}
```

Additional hash commands include `hdel`, `hexists`, `hkeys`, `hvals`, `hlen`, `hincrby`,
`hincrbyfloat`, `hmget`, `hscan`, `hstrlen`, and `hsetnx`.

```v ignore
fields := db.hmget[string]('user:1', 'name', 'missing')!
has_name := db.hexists('user:1', 'name')!
db.hincrby('user:1', 'age', 1)!
db.hincrbyfloat('user:1', 'score', 0.5)!
names := db.hkeys('user:1')!
db.hdel('user:1', 'score')!
```

### Pipeline Operations

The string, key, and hash wrappers support pipelines. While queued, they return zero, false,
empty strings or collections, or the default generic value. Read the actual results from
`pipeline_execute()`, which returns raw `RedisValue` replies in command order, including
`RedisNull` for missing values. Value type and option validation happen before commands are queued.
```v ignore
// Start pipeline
db.pipeline_start()

// Queue commands
db.incr('counter')!
db.set('name', 'Charlie')!
db.get[string]('name')!

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
}

mut db := redis.connect(config)!
defer {
    db.close() or { eprintln('Error closing connection: ${err}') }
}
```

`redis.DB` also implements `pool.ConnectionPoolable`, so it can be used
directly with `vlib/pool` connection pools.

## Performance Tips

1. **Reuse Connections**: Maintain connections instead of creating new ones
2. **Use Pipelines**: Batch commands for high-throughput operations
3. **Prefer Integers**: Use numeric types for counters and metrics
4. **Specify Types**: Always specify return types for get operations
