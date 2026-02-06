# Connection Pool Module

This module provides a robust connection pooling implementation for 
managing reusable resources like database connections. It handles 
connection lifecycle, validation, and efficient resource allocation 
with minimal overhead.

## Features

- **Connection Reuse**: Efficiently manages reusable connections
- **Health Validation**: Automatic connection validation
- **Dynamic Scaling**: Adjusts pool size based on demand
- **Intelligent Eviction**: Removes stale connections with priority-based cleanup
- **Statistics Tracking**: Provides detailed pool metrics
- **Thread Safety**: Fully concurrent-safe implementation
- **Graceful Shutdown**: Clean resource termination
- **Dynamic Configuration**: Runtime configuration updates

## Basic Usage

### Creating a Pool

```v ignore
import db.mysql
import pool
import time

// Define your connection factory function
fn create_conn() !&pool.ConnectionPoolable {
	config := mysql.Config{
		host:     '127.0.0.1'
		port:     3306
		username: 'root'
		password: '12345678'
		dbname:   'mysql'
	}
	db := mysql.connect(config)!
	return &db
}

// Configure pool parameters
config := pool.ConnectionPoolConfig{
	max_conns:      50
	min_idle_conns: 5
	max_lifetime:   2 * time.hour
	idle_timeout:   30 * time.minute
	get_timeout:    5 * time.second
}

// Create connection pool
mut my_pool := pool.new_connection_pool(create_conn, config)!

// Acquire connection
mut conn := my_pool.get()!

// Convert `conn` to a `mysql.DB` object
mut db := conn as mysql.DB

// Use connection `db`
// ... your operations ...
// db.exec()

// Return connection to pool
my_pool.put(conn)!

// When application exits
my_pool.close()
```

## Configuration Options

| Parameter           | Default Value         | Description                          |
|---------------------|-----------------------|--------------------------------------|
| `max_conns`         | 20                    | Maximum connections in pool          |
| `min_idle_conns`    | 5                     | Minimum idle connections to maintain |
| `max_lifetime`      | 1 hour                | Max connection lifetime              |
| `idle_timeout`      | 30 minutes            | Idle connection timeout              |
| `get_timeout`       | 5 seconds             | Connection acquisition timeout       |
| `retry_base_delay`  | 1 second              | Base delay for connection retries    |
| `max_retry_delay`   | 30 seconds            | Maximum retry delay                  |
| `max_retry_attempts`| 5                     | Maximum connection creation attempts |

## Advanced Features

### Dynamic Configuration Update

```v ignore
new_config := pool.ConnectionPoolConfig{
    max_conns: 100
    min_idle_conns: 10
    // ... other parameters ...
}

my_pool.update_config(new_config)!
```

### Connection Recovery Signal

```v ignore
// After connection maintenance/outage
my_pool.signal_recovery_event()
```

### Statistics Monitoring

```v ignore
stats := my_pool.stats()
println("Active connections: ${stats.active_conns}")
println("Idle connections: ${stats.idle_conns}")
println("Waiting clients: ${stats.waiting_clients}")
```

## Implementation Notes

1. **Exponential Backoff**: Connection creation uses exponential backoff with jitter
2. **Priority Eviction**: Four priority levels for connection cleanup:
   - `low`: Routine maintenance
   - `medium`: Connection acquisition failure
   - `high`: Configuration changes
   - `urgent`: Connection recovery events
3. **Adaptive Cleanup**: Maintenance thread dynamically adjusts processing frequency
4. **Wait Queue**: Fair connection allocation to waiting clients
5. **Atomic Operations**: Non-blocking statistics tracking

## Performance Considerations

- Use appropriate `min_idle_conns` to balance startup time and memory
- Set `max_lifetime` according to your backend connection limits
- Monitor `creation_errors` statistic to detect connection issues
- Use `evicted_count` to identify connection health problems

## Example Implementation

```v
// ConnectionPoolable connection interface implementation
struct MyConnection {
	// Your connection state
}

fn (mut c MyConnection) validate() !bool {
	// Connection health check logic
	return true
}

fn (mut c MyConnection) close() ! {
	// Physical close logic
}

fn (mut c MyConnection) reset() ! {
	// Reset connection to initial state
}
```
