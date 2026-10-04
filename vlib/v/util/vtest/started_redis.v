module vtest

// redis_default_port is the port, that a redis-server listens on by default.
// The db.redis tests connect to it.
const redis_default_port = '6379'

// is_default_port_redis_server reports whether `process_line`, a line of `ps ax`,
// is a redis-server, that the db.redis tests can reach on the default port 6379.
// By default, redis-server rewrites its process title to show its listening address
// (`redis-server *:6379`). With `set-proc-title no`, the title keeps the original
// arguments instead, where `--port` can name the port. A server is ruled out only
// by a port that its line shows; `redis-server /etc/redis/redis.conf` counts.
pub fn is_default_port_redis_server(process_line string) bool {
	fields := process_line.fields()
	mut command_index := -1
	for i, field in fields {
		if field.contains('redis-server') {
			command_index = i
			break
		}
	}
	if command_index < 0 {
		return false
	}
	args := fields[command_index + 1..]
	if args.len > 0 {
		listen_address := args[0]
		if listen_address.starts_with('unixsocket:') {
			// A rewritten title of a server without a TCP port.
			return false
		}
		if listen_address.contains(':') {
			shown_port := listen_address.all_after_last(':')
			if shown_port.len > 0 && shown_port.contains_only('0123456789') {
				// The whole port has to match: `*:63790` is another server.
				return shown_port == redis_default_port
			}
		}
	}
	mut port := redis_default_port
	for i := 0; i + 1 < args.len; i++ {
		if args[i] == '--port' {
			port = args[i + 1]
		}
	}
	return port == redis_default_port
}
