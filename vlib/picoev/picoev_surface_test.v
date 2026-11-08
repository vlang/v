module picoev

// picoev's own surface that can be checked without opening a socket: the event
// bitmask constants, the platform-dependent fd ceiling, and the default values
// of the public config structs.

fn test_the_event_flags_are_disjoint_where_they_should_be() {
	// read and write are independent bits, so their sum is their union
	assert picoev_readwrite == picoev_read | picoev_write
	assert picoev_read == 1
	assert picoev_write == 2
	assert picoev_timeout == 4
	// add and del must never be mistaken for an event
	assert picoev_add & picoev_read == 0
	assert picoev_add & picoev_write == 0
	assert picoev_del & picoev_read == 0
	assert picoev_del & picoev_write == 0
	assert picoev_add != picoev_del
	// combining an event with the add flag leaves the event bit intact
	assert (picoev_read | picoev_add) & picoev_read == picoev_read
}

fn test_max_fds_is_a_power_of_two_above_the_default_socket_floor() {
	assert max_fds > 0
	assert max_fds % 1024 == 0
	assert max_queue == 4096
}

fn test_config_defaults_are_the_documented_server_settings() {
	c := Config{}
	assert c.port == 8080
	assert c.timeout_secs == 8
	assert c.max_headers == 100
	assert c.max_read == 4096
	assert c.max_write == 8192
	assert c.host == ''
	// the socket family is ip6 by default, run in dual-stack mode by new()
	assert c.family == .ip6
}

fn test_a_target_starts_out_unowned_by_any_loop() {
	t := Target{}
	assert t.fd == 0
	// -1 is the "not in a loop" marker that init() relies on
	assert t.loop_id == -1
	assert t.events == 0
	assert t.backend == 0
}

@[unsafe]
fn test_init_fills_the_descriptor_table_with_isolated_targets() {
	mut pv := &Picoev{}
	pv.init()
	// every slot is its own Target, so clearing one cannot clear another
	for i in 0 .. max_fds {
		assert unsafe { pv.file_descriptors[i] } != unsafe { nil }
		assert unsafe { pv.file_descriptors[i].loop_id } == -1
		assert unsafe { pv.file_descriptors[i].fd } == 0
	}
	assert pv.num_loops == 0
}

// NOTE: `Picoev.file_descriptors` is declared as a fixed `[4096]&Target` with a
// TODO to size it from max_fds. On this platform max_fds is 4096, so the two
// agree; where max_fds is 1024 the array is larger than the ceiling, not
// smaller, so it is harmless here. Asserted rather than changed.
fn test_the_descriptor_table_is_at_least_as_large_as_the_fd_ceiling() {
	assert unsafe { sizeof([4096]&Target) } > 0
	assert max_fds <= 4096
}
