# Redis Client for V

This module provides a Redis client implementation in V that supports the 
Redis Serialization Protocol (RESP) versions 2 (RESP2) and 3 (RESP3) with
a type-safe interface for common Redis commands.

## Features

- **Typed Commands**: Strings, keys, hashes, lists, sets, sorted sets, and streams
- **Pipelines and Transactions**: Batch execution, optimistic locking, and per-command results
- **Connections**: Timeouts, TLS verification, reconnect controls, and `vlib/pool` integration
- **RESP Protocol Support**: Full Redis Serialization Protocol implementation
- **Advanced Features**: Pub/Sub, bitmaps, HyperLogLog, geospatial search, scripting, and admin APIs
- **Deployment Support**: Redis Cluster routing and Sentinel master discovery

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
Scalar and multi-get unsigned reads preserve the full range of `u64` and `usize`.
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

The typed command wrappers support pipelines. While queued, they return zero, false,
empty strings or collections, or the default generic value. Read the actual results from
`pipeline_execute()`, which returns raw `RedisValue` replies in command order, including
`RedisNull` for missing values. Command errors remain `RedisBlobError` entries, so later replies
are consumed and the connection stays synchronized. Value and option validation happen
before queueing.
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

### Lists, Sets, and Sorted Sets

List commands include pushes and pops, conditional pushes, indexing, range reads, insertion,
removal, position lookup, moves, and multi-key pops. Blocking variants include `blpop`, `brpop`,
`blmove`, and `blmpop`. `ListSide` selects `.left` or `.right`; `ListInsertPosition` selects
`.before` or `.after`. Binary data is preserved in V strings.

```v ignore
db.rpush('queue', 'first', 'second')!
items := db.lrange('queue', 0, -1)!
item := db.lpop('queue')!
popped := db.blpop(['queue'], 0.5)!
// popped is ListPop{key, values}; a blocking timeout returns NilError.
```

Set commands include membership checks, random selection, moving members, intersections,
unions, differences, stored combinations, cardinality queries, and `sscan`.
`smismember` returns membership flags in request order. `sintercard` accepts an optional limit
as its final integer argument (`0` means unlimited).

```v ignore
db.sadd('tags', 'v', 'redis')!
present := db.sismember('tags', 'v')!
members := db.smembers('tags')!
shared := db.sinter('tags', 'other-tags')!
```

Sorted set commands include insertion, score changes, rank/score/lexicographic ranges,
rank and score queries, random members, pops, blocking pops, removals, weighted combinations,
intersection cardinality, and `zscan`. `ZMember` contains `member` and `score`.
Range methods return member strings; `_withscores` variants return `[]ZMember` under both
RESP2 and RESP3. `WeightedKey` selects keys and weights; `ZCombineOptions.aggregate` selects
`.sum`, `.min`, or `.max`.

```v ignore
db.zadd('scores', [redis.ZMember{member: 'Alice', score: 10}])!
db.zincrby('scores', 2.5, 'Alice')!
leaders := db.zrange_withscores('scores', 0, 9, rev: true)!
rank := db.zrevrank('scores', 'Alice')!
```

Single-value pops, missing indices, ranks, and scores return `NilError`. Methods ending in
`_count` return collections and preserve empty results. `zmscore` preserves missing scores as
nullable entries. Set iteration order is unspecified; scans can contain duplicate entries.
`lmpop`, `blmpop`, `sintercard`, and `zintercard` require Redis 7.0 or later.

### Transactions

`multi`, `exec`, `discard`, `watch`, and `unwatch` expose Redis transactions. Typed commands
return placeholders while inside `MULTI`; actual results come from `exec`. A changed watched
key makes `exec` return `NilError`. Runtime errors in individual commands remain
`RedisBlobError` entries in the result array.

```v ignore
db.watch('balance')!
db.multi()!
db.incrby('balance', 10)!
results := db.exec()!
```

`transaction_start` buffers `MULTI` and subsequent commands locally. `transaction_execute`
sends the batch and returns only the `EXEC` result. `discard` cancels either transaction form.
`reset` clears queued bytes, discards server transactions, and removes watched keys before
pool reuse. Reconnects are disabled while a transaction or `WATCH` is active.

### Pub/Sub

Use `connect_pubsub(config)` for a dedicated subscription connection. It supports
`subscribe`, `psubscribe`, `unsubscribe`, `punsubscribe`, and `next_message` under both RESP
versions. `PubSubMessage` carries a channel, optional pattern, and binary payload in `[]u8`.
`listen(callback)` invokes the callback until it returns `false`; callback helpers also
subscribe first. The subscription connection belongs to the caller running the listener.

```v ignore
mut sub := redis.connect_pubsub()!
defer { sub.close() or {} }
sub.subscribe('events')!
// Publish from a separate DB connection.
db.publish('events', 'hello')!
message := sub.next_message()!
println(message.payload.bytestr())
```

### Streams

Stream commands include `xadd`, `xread`, `xreadgroup`, all `xgroup` management variants,
`xrange`, `xrevrange`, `xlen`, `xdel`, `xtrim`, `xack`, `xclaim`, `xautoclaim`, `xpending`,
and `xinfo` variants. `StreamEntry` contains an ID and a field/value map; strings
preserve binary payloads. `StreamOffset` pairs a key with the requested read offset.

```v ignore
id := db.xadd('events', '*', {'kind': 'created'})!
entries := db.xrange('events', '-', '+')!
db.xgroup_create('events', 'workers', '0', mkstream: true)!
reads := db.xreadgroup('workers', 'worker-1', [redis.StreamOffset{key: 'events', id: '>'}])!
db.xack('events', 'workers', id)!
```

Redis blocking timeouts return empty stream collections. `XReadOptions.block` is nullable: omitted
means nonblocking, while `0` blocks indefinitely at Redis. Configure the connection's
`read_timeout` to allow the desired blocking duration. `StreamTrim` supports `MAXLEN` or
`MINID` limits; `XClaimOptions` and `XAutoClaimOptions` select claim behavior. Claim ID-only
variants and deleted IDs are preserved. `xautoclaim` requires Redis 6.2 or later.

### Bitmaps, HyperLogLog, and Geospatial Commands

Bitmap methods include `setbit`, `getbit`, `bitcount`, `bitpos`, `bitop`, `bitfield`, and
`bitfield_ro`. `BitRange` optionally selects byte or bit offsets. `BitFieldOperation` uses
Redis encodings (`i16`, `u8`) and offsets (`0`, `#2`); overflow-failure results remain nullable.
`pfadd`, `pfcount`, and `pfmerge` expose approximate HyperLogLog cardinality.

Geospatial methods include `geoadd`, `geodist`, `geohash`, `geopos`, `geosearch`, and
`geosearchstore`, plus deprecated radius variants. `GeoSearchOptions` selects a center,
circle or box, units, ordering, and optional distance/hash/coordinate metadata.
Missing geohashes and positions are nullable. `geosearch` requires Redis 6.2 or later.

### Scripting and Administration

Scripting methods include `eval`, `evalsha`, read-only variants, `script_load`,
`script_exists`, `script_flush`, `script_kill`, `fcall`, and `fcall_ro`. Redis Function
management supports loading, listing, deleting, flushing, dumping, restoring, statistics,
and killing functions. Function calls require Redis 7.0 or later. Complex replies retain
`RedisValue` shapes, including explicit nulls.

Server methods cover information, configuration, clients, replication, database counts,
time, persistence, slow logs, memory, ACLs, command metadata, and cluster administration.
`server_time` returns seconds and microseconds. RESP3 verbatim text replies are normalized to
strings. ACL methods preserve raw structured replies where needed. Administrative commands
retain their Redis side effects and permission checks.

`monitor` uses a dedicated connection; read events with `monitor_next`. `client_reply('OFF')`
and `client_reply('SKIP')` send without reading a reply. Use a dedicated connection, send any
suppressed commands through its transport, and restore replies with `client_reply('ON')`
before calling other wrappers. `quit` closes the connection; `shutdown` accepts clean EOF
before a response prefix as success and propagates incomplete replies, server errors, and timeouts.

### Custom Commands
```v ignore
// Run raw commands
resp := db.cmd('SET', 'custom', 'value')!
result := db.cmd('GET', 'custom')!

// Complex commands
db.cmd('HSET', 'user:2', 'field1', 'value1', 'field2', '42')!
```

## Error Handling

Commands return result types and preserve these error categories:

- `ConnectionError`: dialing, transport closure, or timeout
- `CommandError`: Redis error replies or invalid command options
- `ProtocolError`: malformed frames or unexpected reply shapes
- `AuthError`: rejected authentication
- `NilError`: a missing single value, blocking-pop timeout, or aborted transaction

Each type implements `RedisError` and V's `IError`; use `err is redis.NilError`, for example.
Transport failures are distinct from server error replies. Detailed messages cover:

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

### Timeouts, TLS, and Reconnects

`Config` exposes `connect_timeout`, `read_timeout`, and `write_timeout` as `time.Duration`.
Defaults are 5 seconds for TCP connection and 30 seconds for reads and writes. TCP connection
timeouts cover the handshake after address resolution. TLS handshakes use `connect_timeout`;
command I/O uses the read/write timeouts. Use `net.infinite_timeout` for unlimited I/O waits.
Blocking Redis operations still need a sufficiently long client read timeout.

Authentication supports `username` (default: `'default'`) and `password`; `database` selects
a logical database during connection. `select_db` changes it and retains it across reconnects.
Named ACL users authenticate even with an empty password, including Redis users with `nopass`.
Successful `auth_user` and `hello` calls with `AUTH` save their credentials for reconnects;
failed authentication leaves the saved credentials unchanged.
`keep_alive` enables TCP keepalive.

TLS configuration includes `tls_validate`, `tls_ca`, `tls_cert`, `tls_key`, `tls_server_name`,
and `tls_in_memory`. Certificate verification remains opt-in for compatibility; enable it
for verified TLS. CA and client credential fields are paths, or PEM strings when
`tls_in_memory` is true. The server name controls SNI and hostname verification.

```v ignore
mut db := redis.connect(
    host: 'redis.example.com'
    tls: true
    tls_validate: true
    tls_ca: '/path/to/ca.pem'
    tls_cert: '/path/to/client.pem'
    tls_key: '/path/to/client.key'
    connect_timeout: 2 * time.second
    read_timeout: 5 * time.second
    write_timeout: 5 * time.second
    auto_reconnect: true
)!
```

`reconnect` restores authentication and the selected database. With `auto_reconnect` enabled,
`validate` repairs a failed idle connection; later commands also reconnect a closed transport.
`max_retries` and `retry_delay` control retries only when a failed write demonstrably sent zero
bytes. Partially sent commands and failed reads are never replayed: their effects are unknown.
Transport or protocol failures close the uncertain connection. Pipelines, transactions, and
watched connections cannot reconnect automatically.

`Redis` is a mutable interface for raw commands, health checks, reset, and close, suitable
for application mocks. Typed wrappers remain methods on `DB`.

### Cluster and Sentinel

`connect_cluster(seeds)` discovers `CLUSTER SLOTS` and caches connections to owning nodes.
`hash_slot` implements Redis CRC16 slots and hash tags. `Cluster.cmd(key, ...args)` uses the
explicit routing key and follows bounded `MOVED` and `ASK` redirects; `ASKING` is sent before
an ASK retry. `Cluster.set` and `Cluster.get` supply that key automatically.
Multi-key commands must use keys with the same slot; automatic cross-slot splitting and
cluster-wide pipelines are not provided. Cluster databases must be zero. Discovered nodes
reuse the successful seed's authentication and TLS settings.

```v ignore
mut cluster := redis.connect_cluster([redis.Config{host: '127.0.0.1', port: 7000}])!
defer { cluster.close() or {} }
cluster.set('{user:1}:name', 'Alice')!
name := cluster.get[string]('{user:1}:name')!
```

`connect_sentinel` accepts `SentinelConfig` with separate Sentinel endpoints/credentials,
`master_name`, and `master_config`. Discovery verifies that the returned node's `ROLE` is
master. `Sentinel.cmd` rediscovers on connection failure for the next call, without replaying
an uncertain command. A rejected `READONLY` command triggers discovery and one safe retry.
Explicit `reconnect` forces discovery. Close the client when finished.

### Concurrent Commands and Instrumentation

`async_client(config).cmd_async(...args)` returns a channel of `AsyncResult`. Each request
runs on its own connection, so these independent calls do not share transaction state.
`AsyncResult.err` retains the original error type; `value` contains the successful reply.
Use `vlib/pool` and separate `DB` instances when concurrent workloads need connection reuse.

`DB.statistics()` returns command counts, failures, reconnects, and cumulative duration.
`Config.trace_hook` receives a `CommandTrace` containing command name, elapsed time, and
failure status. Failures track transport errors and direct Redis error replies; nested transaction
errors remain result values. Traces never include keys, values, passwords, or full arguments.
Each `DB`, `Cluster`, `Sentinel`, or `PubSub` instance belongs to one caller at a time.

## Performance Tips

1. **Reuse Connections**: Maintain connections instead of creating new ones
2. **Use Pipelines**: Batch commands for high-throughput operations
3. **Prefer Integers**: Use numeric types for counters and metrics
4. **Specify Types**: Always specify return types for get operations
