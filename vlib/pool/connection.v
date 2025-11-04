module pool

import sync
import sync.stdatomic { new_atomic }
import time

// Eviction channel capacity
const eviction_ch_cap = 1000

// ConnectionPoolable defines the interface for connection objects
pub interface ConnectionPoolable {
mut:
	// validate checks if the connection is still usable
	validate() !bool
	// close terminates the physical connection
	close() !
	// reset returns the connection to initial state for reuse
	reset() !
}

// ConnectionPoolConfig holds configuration settings for the connection pool
@[params]
pub struct ConnectionPoolConfig {
pub mut:
	max_conns          int           = 20               // Maximum allowed connections
	min_idle_conns     int           = 5                // Minimum idle connections to maintain
	max_lifetime       time.Duration = time.hour        // Max lifetime of a connection
	idle_timeout       time.Duration = 30 * time.minute // Time before idle connections are cleaned up
	get_timeout        time.Duration = 5 * time.second  // Max time to wait for a connection
	retry_base_delay   time.Duration = 1 * time.second  // Base delay for retry backoff
	max_retry_delay    time.Duration = 30 * time.second // Maximum delay for retry backoff
	max_retry_attempts int           = 5                // Maximum retry attempts
}

// ConnectionWrapper contains metadata about a pooled connection
struct ConnectionWrapper {
mut:
	conn          &ConnectionPoolable // The actual connection object
	created_at    time.Time           // When connection was created
	last_used_at  time.Time           // Last time connection was used
	last_valid_at time.Time           // Last time connection was validated
	usage_count   int                 // How many times this connection has been used
}

// EvictionPriority indicates urgency of connection cleanup
pub enum EvictionPriority {
	low    // Routine cleanup (connection return)
	medium // Connection get failure
	high   // Configuration change
	urgent // Database/Server recovery
}

// ConnectionPool manages a pool of reusable connections
pub struct ConnectionPool {
mut:
	config ConnectionPoolConfig
	// Lock order:
	// config_mutex > create_mutex > idle_pool_mutex > all_conns_mutex > wait_queue_mutex
	config_mutex     &sync.RwMutex @[required] // Guards configuration changes
	create_mutex     &sync.Mutex // Serializes connection creation
	idle_pool_mutex  &sync.RwMutex              @[required] // Protects idle connections
	all_conns_mutex  &sync.RwMutex              @[required] // Protects all connections map
	wait_queue_mutex &sync.RwMutex              @[required] // Protects wait queue
	is_closed        &stdatomic.AtomicVal[bool] @[required] // Pool shutdown flag
	stop_ch          chan bool             // Signals maintenance thread to stop
	eviction_ch      chan EvictionPriority // Eviction event channel
	cleanup_thread   thread                // Background maintenance thread
	wait_queue       []chan bool           // Client wait queue for connection acquisition
	conn_factory     fn () !&ConnectionPoolable @[required] // Creates new connections
	active_count     &stdatomic.AtomicVal[int]  @[required] // Currently checked-out connections
	created_at       time.Time                      // Pool creation timestamp
	all_conns        map[voidptr]&ConnectionWrapper // All tracked connections
	idle_pool        []&ConnectionWrapper           // Currently idle connections
	creation_errors  &stdatomic.AtomicVal[int] @[required] // Failed creation attempts
	evicted_count    &stdatomic.AtomicVal[int] @[required] // Connections forcibly removed
	creating_count   &stdatomic.AtomicVal[int] @[required] // Connections being created
}

// new_connection_pool creates a new connection pool
pub fn new_connection_pool(conn_factory fn () !&ConnectionPoolable, config ConnectionPoolConfig) !&ConnectionPool {
	// Validate configuration parameters
	check_config(config)!

	mut p := &ConnectionPool{
		conn_factory:     conn_factory
		config:           config
		config_mutex:     sync.new_rwmutex()
		create_mutex:     sync.new_mutex()
		idle_pool_mutex:  sync.new_rwmutex()
		all_conns_mutex:  sync.new_rwmutex()
		wait_queue_mutex: sync.new_rwmutex()
		is_closed:        new_atomic(false)
		stop_ch:          chan bool{cap: 1}
		eviction_ch:      chan EvictionPriority{cap: eviction_ch_cap}
		active_count:     new_atomic(0)
		creation_errors:  new_atomic(0)
		evicted_count:    new_atomic(0)
		creating_count:   new_atomic(0)
		all_conns:        map[voidptr]&ConnectionWrapper{}
	}

	now := time.utc()
	p.created_at = now

	// Initialize minimum idle connections
	for _ in 0 .. config.min_idle_conns {
		conn := p.create_conn_with_retry() or {
			// Cleanup if initialization fails
			p.all_conns_mutex.lock()
			for _, mut wrapper in p.all_conns {
				wrapper.conn.close() or {}
			}
			p.all_conns.clear()
			p.all_conns_mutex.unlock()
			return err
		}
		wrapper := &ConnectionWrapper{
			conn:          conn
			created_at:    now
			last_used_at:  now
			last_valid_at: now
		}
		p.idle_pool << wrapper
		p.all_conns_mutex.lock()
		p.all_conns[conn] = wrapper
		p.all_conns_mutex.unlock()
	}

	// Start background maintenance thread
	p.cleanup_thread = spawn p.background_maintenance()
	// Initial connection pruning
	p.prune_connections()
	return p
}

// create_conn_with_retry creates a connection with exponential backoff retries
fn (mut p ConnectionPool) create_conn_with_retry() !&ConnectionPoolable {
	// Get current configuration
	p.config_mutex.rlock()
	max_attempts := p.config.max_retry_attempts
	base_delay := p.config.retry_base_delay
	max_delay := p.config.max_retry_delay
	p.config_mutex.unlock()

	// Serialize connection creation
	p.create_mutex.lock()
	defer {
		p.create_mutex.unlock()
	}

	mut attempt := 0
	p.creating_count.add(1)
	defer {
		p.creating_count.sub(1)
	}

	for {
		mut conn := p.conn_factory() or {
			// Handle creation error with exponential backoff
			if attempt >= max_attempts {
				return error('Connection creation failed after ${attempt} attempts: ${err}')
			}

			// Calculate next delay with exponential backoff
			mut delay := base_delay * time.Duration(1 << attempt)
			if delay > max_delay {
				delay = max_delay
			}

			time.sleep(delay)
			attempt++
			p.creation_errors.add(1)
			continue
		}

		// Validate new connection
		if !conn.validate() or { false } {
			conn.close() or {}
			return error('New connection validation failed')
		}
		return conn
	}
	return error('Unreachable code')
}

// try_wakeup_waiters attempts to notify waiting clients of available resources
fn (mut p ConnectionPool) try_wakeup_waiters() {
	can_create := p.can_create()
	p.wait_queue_mutex.lock()
	defer {
		p.wait_queue_mutex.unlock()
	}

	// Notify first client if resources are available
	if p.wait_queue.len > 0 {
		if p.idle_pool.len > 0 || can_create {
			to_wake := p.wait_queue[0]
			p.wait_queue.delete(0)
			to_wake <- true
		}
	}
}

// can_create checks if new connections can be created
@[inline]
fn (mut p ConnectionPool) can_create() bool {
	p.config_mutex.rlock()
	max_conns := p.config.max_conns
	p.config_mutex.unlock()
	return p.active_count.load() + p.creating_count.load() < max_conns && !p.is_closed.load()
		&& p.all_conns.len < max_conns
}

// get acquires a connection from the pool with timeout
pub fn (mut p ConnectionPool) get() !&ConnectionPoolable {
	start_time := time.utc()
	for {
		// Check if pool is closed
		if p.is_closed.load() {
			return error('Connection pool closed')
		}

		// Try immediate acquisition
		if conn := p.try_get() {
			p.eviction_ch <- .medium
			return conn
		}

		// Check if new connection can be created
		can_create := p.can_create()
		if can_create {
			mut new_conn := p.create_conn_with_retry()!

			// Final check before adding to pool
			if p.is_closed.load() {
				new_conn.close()!
				return error('Connection pool closed')
			}

			p.config_mutex.rlock()
			max_conns := p.config.max_conns
			p.config_mutex.unlock()

			p.all_conns_mutex.lock()
			if p.all_conns.len < max_conns {
				// Successfully create and add new connection
				now := time.utc()
				wrapper := &ConnectionWrapper{
					conn:          new_conn
					created_at:    now
					last_used_at:  now
					last_valid_at: now
				}
				p.all_conns[new_conn] = wrapper
				p.all_conns_mutex.unlock()
				p.active_count.add(1)
				return new_conn
			} else {
				// Connection limit reached - close new connection
				p.all_conns_mutex.unlock()
				new_conn.close()!
			}
		}

		// Second attempt to get connection
		if conn := p.try_get() {
			return conn
		}

		// Calculate remaining time for connection acquisition
		p.config_mutex.rlock()
		timeout := p.config.get_timeout
		p.config_mutex.unlock()
		elapsed := time.utc() - start_time
		if elapsed > timeout {
			return error('Connection acquisition timeout')
		}
		remaining := timeout - elapsed

		// Set up notification channel
		notify_chan := chan bool{cap: 1}
		defer {
			notify_chan.close()
		}

		// Add to wait queue
		p.wait_queue_mutex.lock()
		p.wait_queue << notify_chan
		p.wait_queue_mutex.unlock()

		select {
			_ := <-notify_chan {
				// Notification received - retry acquisition
			}
			i64(remaining) {
				// Timeout cleanup
				p.wait_queue_mutex.lock()
				for i := 0; i < p.wait_queue.len; i++ {
					if p.wait_queue[i] == notify_chan {
						p.wait_queue.delete(i)
						break
					}
				}
				p.wait_queue_mutex.unlock()
				if conn := p.try_get() {
					return conn
				}
				return error('Connection acquisition timeout')
			}
		}
	}

	return error('Unreachable code')
}

// try_get attempts non-blocking connection acquisition
fn (mut p ConnectionPool) try_get() ?&ConnectionPoolable {
	// Get relevant configuration parameters
	p.config_mutex.rlock()
	min_idle := p.config.min_idle_conns
	max_lifetime := p.config.max_lifetime
	p.config_mutex.unlock()

	p.idle_pool_mutex.lock()
	defer {
		p.idle_pool_mutex.unlock()
	}

	// Determine eviction priority based on idle count
	priority := if p.idle_pool.len <= min_idle {
		EvictionPriority.urgent
	} else if p.idle_pool.len > min_idle * 2 {
		EvictionPriority.low
	} else {
		EvictionPriority.medium
	}
	p.eviction_ch <- priority

	// Process idle connections
	for p.idle_pool.len > 0 {
		mut wrapper := p.idle_pool.pop()

		// Check connection lifetime
		age := time.utc() - wrapper.created_at
		if age > max_lifetime {
			// Close expired connection
			p.all_conns_mutex.lock()
			p.all_conns.delete(wrapper.conn)
			p.all_conns_mutex.unlock()
			wrapper.conn.close() or {}
			continue
		}

		// Validate connection
		if !wrapper.conn.validate() or { false } {
			// Handle invalid connection
			p.all_conns_mutex.lock()
			p.all_conns.delete(wrapper.conn)
			p.all_conns_mutex.unlock()
			wrapper.conn.close() or {}
			continue
		}

		wrapper.last_valid_at = time.utc()

		// Mark connection as active
		p.active_count.add(1)
		wrapper.last_used_at = time.utc()
		wrapper.usage_count++
		return wrapper.conn
	}
	return none
}

// put returns a connection to the pool
pub fn (mut p ConnectionPool) put(conn &ConnectionPoolable) ! {
	if p.active_count.load() > 0 {
		// TODO: may need a atomic check here, compare_exchange?
		p.active_count.sub(1)
	}

	mut conn_ptr := unsafe { conn }
	// Handle closed pool case
	if p.is_closed.load() {
		conn_ptr.close()!
		return
	}

	// Reset connection to initial state
	conn_ptr.reset() or {
		conn_ptr.close() or {}
		p.all_conns_mutex.lock()
		p.all_conns.delete(conn)
		p.all_conns_mutex.unlock()
		return err
	}

	p.idle_pool_mutex.lock()
	p.all_conns_mutex.lock()
	defer {
		p.all_conns_mutex.unlock()
		p.idle_pool_mutex.unlock()
	}

	// Return connection to idle pool
	if mut wrapper := p.all_conns[conn] {
		wrapper.last_used_at = time.utc()
		p.idle_pool << wrapper

		// Determine if eviction is needed
		p.config_mutex.rlock()
		low_eviction := p.idle_pool.len > p.config.min_idle_conns
		p.config_mutex.unlock()

		// Wake any waiting clients
		p.try_wakeup_waiters()

		// Trigger eviction if needed
		priority := if low_eviction { EvictionPriority.low } else { EvictionPriority.urgent }
		p.eviction_ch <- priority
	} else {
		// Handle unmanaged connection
		conn_ptr.close()!
		return error('Unmanaged connection')
	}
}

// close shuts down the connection pool and cleans up resources
pub fn (mut p ConnectionPool) close() {
	if p.is_closed.load() {
		return
	}
	p.is_closed.store(true)

	// Signal background thread to stop
	p.stop_ch <- true
	p.cleanup_thread.wait()
	p.stop_ch.close()

	// Close all active connections
	p.idle_pool_mutex.lock()
	p.all_conns_mutex.lock()
	for _, mut wrapper in p.all_conns {
		wrapper.conn.close() or {}
	}
	p.all_conns.clear()
	p.idle_pool.clear()
	p.all_conns_mutex.unlock()
	p.idle_pool_mutex.unlock()

	// Process clients in the wait queue
	p.wait_queue_mutex.lock()
	waiters := p.wait_queue.clone()
	p.wait_queue.clear()
	p.wait_queue_mutex.unlock()

	for ch in waiters {
		ch <- true // Notify all waiting clients
	}

	p.eviction_ch.close()

	// Reset all counters
	p.active_count.store(0)
	p.creation_errors.store(0)
	p.evicted_count.store(0)
	p.creating_count.store(0)
}

// background_maintenance handles periodic connection cleanup
fn (mut p ConnectionPool) background_maintenance() {
	mut first_trigger_time := u64(0)
	mut event_count := 0
	mut min_interval := time.infinite // Dynamic processing interval

	for {
		// Calculate adaptive processing interval
		p.config_mutex.rlock()
		dynamic_interval := if p.config.idle_timeout / 10 > time.second {
			time.second
		} else {
			p.config.idle_timeout / 10
		}
		p.config_mutex.unlock()

		interval := if min_interval < dynamic_interval {
			min_interval
		} else {
			dynamic_interval
		}

		select {
			_ := <-p.stop_ch {
				// Termination signal received
				return
			}
			priority := <-p.eviction_ch {
				// Process event based on priority
				match priority {
					.low {
						event_count++
						min_interval = 100 * time.millisecond
					}
					.medium {
						event_count += 10
						min_interval = 10 * time.millisecond
					}
					.high {
						event_count += 50
						min_interval = 1 * time.millisecond
					}
					.urgent {
						event_count += 1000
						min_interval = 100 * time.microsecond
					}
				}

				// Track first event time
				if first_trigger_time == 0 {
					first_trigger_time = time.sys_mono_now()
				}

				elapsed := time.sys_mono_now() - first_trigger_time

				// Determine if immediate processing is needed
				if priority == .urgent
					|| (priority == .high && elapsed > 100 * time.microsecond)
					|| (priority == .medium && elapsed > 1 * time.millisecond)
					|| (priority == .low && elapsed > 10 * time.millisecond)
					|| event_count >= 1000 {
					p.prune_connections()
					event_count = 0
					first_trigger_time = 0
					min_interval = time.infinite
				}
			}
			i64(interval) {
				// Periodic maintenance
				if event_count > 0 || interval == min_interval {
					p.prune_connections()
					event_count = 0
					first_trigger_time = 0
					min_interval = time.infinite
				}
			}
		}
	}
}

// prune_connections removes invalid connections and maintains min idle count
fn (mut p ConnectionPool) prune_connections() {
	// Get current configuration parameters
	p.config_mutex.rlock()
	max_lifetime := p.config.max_lifetime
	idle_timeout := p.config.idle_timeout
	min_idle := p.config.min_idle_conns
	p.config_mutex.unlock()

	p.idle_pool_mutex.lock()
	// Remove stale connections
	for i := p.idle_pool.len - 1; i >= 0; i-- {
		mut wrapper := p.idle_pool[i]
		age := time.utc() - wrapper.created_at
		idle_time := time.utc() - wrapper.last_used_at

		if age > max_lifetime || idle_time > idle_timeout || !wrapper.conn.validate() or { false } {
			p.all_conns_mutex.lock()
			p.all_conns.delete(wrapper.conn)
			p.all_conns_mutex.unlock()
			wrapper.conn.close() or {}
			p.idle_pool.delete(i)
			p.evicted_count.add(1)
		} else {
			wrapper.last_valid_at = time.utc()
		}
	}
	current_idle := p.idle_pool.len
	p.idle_pool_mutex.unlock()

	// Calculate connections to create
	to_create := if min_idle > current_idle { min_idle - current_idle } else { 0 }

	// Create needed connections
	mut new_conns := []&ConnectionPoolable{}
	if to_create > 0 {
		for _ in 0 .. to_create {
			if new_conn := p.create_conn_with_retry() {
				new_conns << new_conn
			}
		}
	}

	// Check if pool was closed during creation
	if p.is_closed.load() {
		for mut new_conn in new_conns {
			new_conn.close() or {}
		}
		return
	}

	p.config_mutex.rlock()
	current_min_idle := p.config.min_idle_conns
	max_conns := p.config.max_conns
	p.config_mutex.unlock()

	// Add new connections to the pool
	p.idle_pool_mutex.lock()
	p.all_conns_mutex.lock()
	defer {
		p.all_conns_mutex.unlock()
		p.idle_pool_mutex.unlock()
	}

	actual_needed := if current_min_idle > p.idle_pool.len {
		current_min_idle - p.idle_pool.len
	} else {
		0
	}
	available_slots := max_conns - p.all_conns.len
	mut actual_to_add := if actual_needed > new_conns.len { new_conns.len } else { actual_needed }
	actual_to_add = if actual_to_add > available_slots { available_slots } else { actual_to_add }

	// Create wrapper for each new connection
	for i in 0 .. actual_to_add {
		now := time.utc()
		wrapper := &ConnectionWrapper{
			conn:          new_conns[i]
			created_at:    now
			last_used_at:  now
			last_valid_at: now
		}
		p.idle_pool << wrapper
		p.all_conns[new_conns[i]] = wrapper
	}

	// Close any extra connections
	for i in actual_to_add .. new_conns.len {
		new_conns[i].close() or {}
	}

	// Wake clients if connections were added
	if actual_to_add > 0 {
		p.try_wakeup_waiters()
	}
}

fn check_config(config ConnectionPoolConfig) ! {
	if config.max_conns <= 0 {
		return error('max_conns must be positive')
	}
	if config.min_idle_conns < 0 {
		return error('min_idle_conns cannot be negative')
	}
	if config.min_idle_conns > config.max_conns {
		return error('min_idle_conns cannot exceed max_conns')
	}
	if config.max_lifetime < 0 {
		return error('max_lifetime cannot be negative')
	}
	if config.idle_timeout < 0 {
		return error('idle_timeout cannot be negative')
	}
	if config.idle_timeout > config.max_lifetime {
		return error('idle_timeout cannot exceed max_lifetime')
	}
	if config.get_timeout < 0 {
		return error('get_timeout cannot be negative')
	}
	if config.retry_base_delay < 0 {
		return error('retry_base_delay cannot be negative')
	}
	if config.max_retry_delay < 0 {
		return error('max_retry_delay cannot be negative')
	}
	if config.max_retry_attempts < 0 {
		return error('max_retry_attempts cannot be negative')
	}
}

// update_config changes the connection pool configuration
pub fn (mut p ConnectionPool) update_config(config ConnectionPoolConfig) ! {
	// Validate configuration
	check_config(config)!
	// Check pool status
	if p.is_closed.load() {
		return error('Connection pool is closed')
	}

	// Update configuration
	p.config_mutex.lock()
	p.config = config
	p.config_mutex.unlock()

	// Trigger maintenance
	p.eviction_ch <- .high
}

// signal_recovery_event notifies the pool of recovery event
pub fn (mut p ConnectionPool) signal_recovery_event() {
	p.eviction_ch <- .urgent
}

// send_eviction triggers a cleanup event
pub fn (mut p ConnectionPool) send_eviction(priority EvictionPriority) {
	p.eviction_ch <- priority
}

// ConnectionPoolStats holds statistics about the pool
pub struct ConnectionPoolStats {
pub:
	total_conns     int       // All managed connections
	active_conns    int       // Currently checked-out connections
	idle_conns      int       // Available connections
	waiting_clients int       // Clients waiting for a connection
	evicted_count   int       // Connections forcibly removed
	creation_errors int       // Failed creation attempts
	created_at      time.Time // When pool was created
	creating_count  int       // Connections being created
}

// stats retrieves current connection pool statistics
pub fn (mut p ConnectionPool) stats() ConnectionPoolStats {
	p.idle_pool_mutex.rlock()
	p.all_conns_mutex.rlock()
	p.wait_queue_mutex.rlock()
	defer {
		p.wait_queue_mutex.unlock()
		p.all_conns_mutex.unlock()
		p.idle_pool_mutex.unlock()
	}

	return ConnectionPoolStats{
		total_conns:     p.all_conns.len
		active_conns:    p.active_count.load()
		idle_conns:      p.idle_pool.len
		waiting_clients: p.wait_queue.len
		evicted_count:   p.evicted_count.load()
		creation_errors: p.creation_errors.load()
		created_at:      p.created_at
		creating_count:  p.creating_count.load()
	}
}
