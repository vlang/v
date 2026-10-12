module pool

struct ShrinkTestConnection {}

fn (mut c ShrinkTestConnection) validate() !bool {
	return true
}

fn (mut c ShrinkTestConnection) close() ! {}

fn (mut c ShrinkTestConnection) reset() ! {}

fn test_maintenance_after_lowering_max_connections() {
	factory := fn () !&ConnectionPoolable {
		return &ShrinkTestConnection{}
	}
	mut p := new_connection_pool(factory, ConnectionPoolConfig{
		max_conns:      5
		min_idle_conns: 3
	})!
	defer {
		p.close()
	}
	assert p.stats().total_conns == 3
	p.update_config(ConnectionPoolConfig{
		max_conns:      2
		min_idle_conns: 1
	})!
	// Invoke maintenance directly so that the regression does not depend on thread timing.
	p.prune_connections()
	assert p.stats().total_conns == 3
	assert p.stats().idle_conns == 3
	conn := p.get()!
	p.put(conn)!
	assert p.stats().total_conns == 3
}
