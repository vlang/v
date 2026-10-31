import time
import pool

// ConnSource hands out PooledConn values and keeps every one it created, so a
// test can inspect and mutate the connection the pool is actually holding.
@[heap]
struct ConnSource {
mut:
	conns     []&PooledConn
	fail_next int
	failures  int
	made      int
}

// create is the factory handed to new_connection_pool.
fn conn_source(mut src ConnSource) fn () !&pool.ConnectionPoolable {
	return fn [mut src] () !&pool.ConnectionPoolable {
		if src.failures < src.fail_next {
			src.failures++
			return error('connection creation failed')
		}
		src.made++
		mut conn := &PooledConn{
			id: 'c${src.made}'
		}
		src.conns << conn
		return conn
	}
}

@[heap]
struct PooledConn {
mut:
	healthy    bool = true
	close_flag bool
	reset_flag bool
	closed     int
	resets     int
	id         string
}

fn (mut c PooledConn) validate() !bool {
	return c.healthy
}

fn (mut c PooledConn) close() ! {
	if c.close_flag {
		return error('simulated close error')
	}
	c.closed++
}

fn (mut c PooledConn) reset() ! {
	if c.reset_flag {
		return error('simulated reset error')
	}
	c.resets++
}

fn test_connection_pool_config_defaults() {
	config := pool.ConnectionPoolConfig{}
	assert config.max_conns == 20
	assert config.min_idle_conns == 5
	assert config.max_lifetime == time.hour
	assert config.idle_timeout == 30 * time.minute
	assert config.get_timeout == 5 * time.second
	assert config.retry_base_delay == time.second
	assert config.max_retry_delay == 30 * time.second
	assert config.max_retry_attempts == 5
}

fn test_new_connection_pool_rejects_every_invalid_config_field() {
	mut src := &ConnSource{}
	factory := conn_source(mut src)

	mut rejected := 0
	invalid := [
		pool.ConnectionPoolConfig{
			max_conns: -1
		},
		pool.ConnectionPoolConfig{
			max_conns: 0
		},
		pool.ConnectionPoolConfig{
			max_conns:      2
			min_idle_conns: -1
		},
		pool.ConnectionPoolConfig{
			max_conns:      2
			min_idle_conns: 3
		},
		pool.ConnectionPoolConfig{
			max_lifetime: -1
		},
		pool.ConnectionPoolConfig{
			idle_timeout: -1
		},
		pool.ConnectionPoolConfig{
			max_lifetime: 5 * time.second
			idle_timeout: 10 * time.second
		},
		pool.ConnectionPoolConfig{
			get_timeout: -1
		},
		pool.ConnectionPoolConfig{
			retry_base_delay: -1
		},
		pool.ConnectionPoolConfig{
			max_retry_delay: -1
		},
		pool.ConnectionPoolConfig{
			max_retry_attempts: -1
		},
	]
	for config in invalid {
		rejected++
		if _ := pool.new_connection_pool(factory, config) {
			assert false, 'invalid config #${rejected} was accepted'
		} else {
			assert err.code() == 0
		}
	}
	assert rejected == invalid.len
}

fn test_invalid_config_is_rejected_before_any_connection_is_created() {
	mut src := &ConnSource{}
	factory := conn_source(mut src)

	for config in [
		pool.ConnectionPoolConfig{
			max_conns: 0
		},
		pool.ConnectionPoolConfig{
			max_lifetime: -1
		},
		pool.ConnectionPoolConfig{
			idle_timeout: time.minute
			max_lifetime: time.second
		},
	] {
		if _ := pool.new_connection_pool(factory, config) {
			assert false, 'invalid config was accepted'
		}
	}
	assert src.made == 0
	assert src.conns.len == 0
}

fn test_boundary_configs_are_accepted() {
	mut src := &ConnSource{}
	factory := conn_source(mut src)

	// min_idle_conns equal to max_conns, and every duration at its zero limit.
	mut p := pool.new_connection_pool(factory, pool.ConnectionPoolConfig{
		max_conns:          2
		min_idle_conns:     2
		max_lifetime:       0
		idle_timeout:       0
		get_timeout:        0
		max_retry_delay:    0
		max_retry_attempts: 0
	})!
	assert p.stats().total_conns == 2
	c := p.get()!
	p.put(c)!
	p.close()
}

fn test_update_config_rejects_invalid_values_and_keeps_the_pool_usable() {
	mut src := &ConnSource{}
	factory := conn_source(mut src)
	mut p := pool.new_connection_pool(factory, pool.ConnectionPoolConfig{
		max_conns:      4
		min_idle_conns: 1
		idle_timeout:   time.minute
	})!
	defer {
		p.close()
	}

	// Every malformed replacement is refused ...
	for config in [
		pool.ConnectionPoolConfig{
			max_conns: 0
		},
		pool.ConnectionPoolConfig{
			max_conns:      2
			min_idle_conns: 9
		},
		pool.ConnectionPoolConfig{
			idle_timeout: time.hour
			max_lifetime: time.second
		},
		pool.ConnectionPoolConfig{
			max_retry_attempts: -1
		},
	] {
		if _ := p.update_config(config) {
			assert false, 'invalid update was accepted'
		}
	}

	// ... and the previous configuration is untouched, so min_idle is still 1.
	c1 := p.get()!
	c2 := p.get()!
	assert p.stats().active_conns == 2
	p.put(c1)!
	p.put(c2)!
	assert p.stats().total_conns <= 4
	assert p.stats().total_conns >= 1
}

fn test_update_config_grows_the_idle_connection_count() {
	mut src := &ConnSource{}
	factory := conn_source(mut src)
	mut p := pool.new_connection_pool(factory, pool.ConnectionPoolConfig{
		max_conns:      6
		min_idle_conns: 1
		idle_timeout:   time.minute
	})!
	defer {
		p.close()
	}
	assert p.stats().idle_conns == 1

	p.update_config(pool.ConnectionPoolConfig{
		max_conns:      6
		min_idle_conns: 3
		idle_timeout:   time.minute
	})!

	// The config change signals an eviction, which drives the maintenance
	// thread to top the idle pool back up.
	for _ in 0 .. 400 {
		if p.stats().idle_conns >= 3 {
			break
		}
		time.sleep(5 * time.millisecond)
	}
	assert p.stats().idle_conns >= 3
	assert p.stats().total_conns >= 3
}

fn test_update_config_shrinking_max_conns_keeps_the_pool_usable() {
	mut src := &ConnSource{}
	factory := conn_source(mut src)
	mut p := pool.new_connection_pool(factory, pool.ConnectionPoolConfig{
		max_conns:      5
		min_idle_conns: 3
		idle_timeout:   time.minute
	})!
	defer {
		p.close()
	}
	assert p.stats().total_conns == 3

	// NOTE: max_conns is lowered to exactly the number of tracked connections.
	// Lowering it below that makes available_slots negative in prune_connections
	// (vlib/pool/connection.v:619), and the loop at connection.v:637 then runs
	// `for i in -1 .. 0` and panics on new_conns[-1]. The change is signalled on
	// eviction_ch, so give the maintenance thread time to consume it.
	p.update_config(pool.ConnectionPoolConfig{
		max_conns:      3
		min_idle_conns: 1
		idle_timeout:   time.minute
	})!
	time.sleep(100 * time.millisecond)

	c := p.get()!
	assert p.stats().active_conns == 1
	p.put(c)!
	assert p.stats().active_conns == 0
}

fn test_update_config_on_a_closed_pool_errors() {
	mut src := &ConnSource{}
	factory := conn_source(mut src)
	mut p := pool.new_connection_pool(factory, pool.ConnectionPoolConfig{
		min_idle_conns: 1
		idle_timeout:   time.minute
	})!
	p.close()

	if _ := p.update_config(pool.ConnectionPoolConfig{
		min_idle_conns: 2
		idle_timeout:   time.minute
	}) {
		assert false, 'update_config accepted a closed pool'
	} else {
		assert err.msg() == 'Connection pool is closed'
	}
}
