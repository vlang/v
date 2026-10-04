module vtest

fn test_redis_server_with_a_rewritten_title_on_the_default_port() {
	assert is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server *:6379')
	assert is_default_port_redis_server('12345   ??  Ss     0:01.23 /opt/homebrew/opt/redis/bin/redis-server 127.0.0.1:6379')
	assert is_default_port_redis_server('  321 ?        Ssl    0:00 redis-server ::1:6379')
	assert is_default_port_redis_server('  321 ?        Ssl    0:00 redis-server *:6379 [cluster]')
}

fn test_redis_server_with_a_rewritten_title_on_another_port() {
	// The default port is a prefix of these ports.
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server *:63790')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server 127.0.0.1:63799')
	// A server, that another tool started on a random port.
	assert !is_default_port_redis_server('12345   ??  Ss     0:01.23 /usr/bin/redis-server 127.0.0.1:54321')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server *:26379 [sentinel]')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server *:637')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server unixsocket:/tmp/redis.sock')
}

fn test_redis_server_without_a_rewritten_title() {
	// `set-proc-title no` keeps the original command line, which may not show the port.
	assert is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server /tmp/redis.conf')
	assert is_default_port_redis_server(' 1234 ?        Ssl    0:10 /usr/bin/redis-server /etc/redis/redis.conf')
	assert is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server')
	assert is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server --port 6379 --set-proc-title no')
	assert is_default_port_redis_server(' 1234 ?        Ssl    0:10 /src/redis-server-8.0/src/redis-server /tmp/redis.conf')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server --port 63790 --set-proc-title no')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 redis-server /tmp/redis.conf --port 7000')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 /src/redis-server-8.0/src/redis-server *:7000')
}

fn test_other_processes_are_not_redis_servers() {
	assert !is_default_port_redis_server('')
	assert !is_default_port_redis_server('  PID TTY      STAT   TIME COMMAND')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 mysqld --port 6379')
	assert !is_default_port_redis_server(' 1234 ?        Ssl    0:10 nc -l 127.0.0.1:6379')
}
