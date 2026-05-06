module quic

// QUIC connection migration operations on Connection.
import net

// migrate_connection migrates the QUIC connection to a new network path.
pub fn (mut c Connection) migrate_connection(new_addr string) ! {
	c.ensure_open()!

	addr_parts := new_addr.split(':')
	if addr_parts.len != 2 {
		return error('invalid address format, expected host:port')
	}
	host := addr_parts[0]

	new_remote_addrs := net.resolve_addrs(host, .ip, .udp) or {
		return error('failed to resolve address: ${err}')
	}
	if new_remote_addrs.len == 0 {
		return error('no addresses resolved for ${host}')
	}

	local_addr := c.migration.current_path.local_addr
	c.migration.probe_path(local_addr, new_remote_addrs[0]) or {
		return error('failed to probe new path: ${err}')
	}
}

// complete_migration completes a pending migration after receiving a PATH_RESPONSE.
pub fn (mut c Connection) complete_migration(response PathResponse) ! {
	c.ensure_open()!

	if c.migration.alternative_paths.len == 0 {
		return error('no pending migration')
	}

	last_path := c.migration.alternative_paths.last()
	validated := c.migration.validate_path(last_path, response) or {
		return error('path validation failed: ${err}')
	}

	if !validated {
		return error('path response does not match challenge')
	}

	for p in c.migration.alternative_paths {
		if p.validated {
			c.migration.migrate_to_path(p) or { return error('failed to migrate to path: ${err}') }
			c.remote_addr = c.migration.current_path.remote_addr.str()
			c.update_ngtcp2_path()
			return
		}
	}

	return error('no validated path found after validation')
}

// check_path_degradation checks if the current network path has degraded.
pub fn (c &Connection) check_path_degradation() bool {
	return c.migration.detect_path_degradation(0.0, c.migration.current_path.rtt)
}

// update_ngtcp2_path updates the ngtcp2 path struct after migration.
// Non-fatal: logs a warning on failure since migration state is already updated.
fn (mut c Connection) update_ngtcp2_path() {
	if c.ngtcp2_conn == unsafe { nil } {
		return
	}
	addr_parts := c.remote_addr.split(':')
	if addr_parts.len != 2 {
		return
	}
	host := addr_parts[0]
	port := addr_parts[1].int()
	rv := C.quic_resolve_and_set_path(&c.path, &c.path_addrs, &char(host.str), port)
	if rv != 0 {
		eprintln('warning: ngtcp2 path update failed after migration')
	}
}
