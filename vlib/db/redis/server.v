module redis

// ServerTime is Redis's Unix timestamp with microsecond precision.
pub struct ServerTime {
pub:
	seconds      i64
	microseconds i64
}

fn (mut db DB) close_uncertain_server_transport(err IError) {
	if err is ConnectionError || err is ProtocolError {
		db.close() or {}
	}
}

fn (mut db DB) execute_server_text(args []string) !string {
	response := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return ''
	}
	if response is RedisVerbatim {
		return response.data.bytestr()
	}
	return bulk_value[string](response, args[0].to_lower())
}

fn server_string_map(value RedisValue, command string) !map[string]string {
	mut result := map[string]string{}
	match value {
		map[string]RedisValue {
			for key, item in value {
				result[key] = bulk_value[string](item, command)!
			}
		}
		else {
			pairs := if value is RedisMap { value.pairs } else { array_value(value, command)! }
			if pairs.len % 2 != 0 {
				return ProtocolError{ message: '`${command}()`: invalid key/value response' }
			}
			for index := 0; index < pairs.len; index += 2 {
				result[bulk_value[string](pairs[index], command)!] = bulk_value[string](pairs[index + 1], command)!
			}
		}
	}
	return result
}

// info returns server information for the requested sections, or the default sections.
pub fn (mut db DB) info(sections ...string) !string {
	mut args := ['INFO']
	args << sections
	return db.execute_server_text(args)
}

// config_get returns matching configuration names and values under either RESP version.
pub fn (mut db DB) config_get(patterns ...string) !map[string]string {
	mut args := ['CONFIG', 'GET']
	args << patterns
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return map[string]string{}
	}
	return server_string_map(resp, 'config_get')
}

// config_set updates server configuration parameters.
pub fn (mut db DB) config_set(parameters map[string]string) !string {
	mut args := ['CONFIG', 'SET']
	for key, value in parameters {
		args << key
		args << value
	}
	return db.execute_string(args)
}

// config_rewrite rewrites the server's configuration file with current parameters.
pub fn (mut db DB) config_rewrite() !string {
	return db.execute_string(['CONFIG', 'REWRITE'])
}

// config_resetstat resets server statistics.
pub fn (mut db DB) config_resetstat() !string {
	return db.execute_string(['CONFIG', 'RESETSTAT'])
}

// client_id retrieves the current connection's unique client identifier.
pub fn (mut db DB) client_id() !i64 {
	return db.execute_i64(['CLIENT', 'ID'])
}

// client_getname retrieves the connection name, or an empty string when unnamed.
pub fn (mut db DB) client_getname() !string {
	resp := db.cmd('CLIENT', 'GETNAME')!
	if db.pipeline_mode || db.transaction_mode || resp is RedisNull {
		return ''
	}
	return bulk_value[string](resp, 'client_getname')
}

// client_setname assigns a name to the current connection.
pub fn (mut db DB) client_setname(name string) !string {
	return db.execute_string(['CLIENT', 'SETNAME', name])
}

// client_list returns connected clients, with optional Redis TYPE or ID filters.
pub fn (mut db DB) client_list(filters ...string) !string {
	mut args := ['CLIENT', 'LIST']
	args << filters
	return db.execute_server_text(args)
}

// client_kill disconnects clients selected by Redis's address or filter arguments.
// The legacy address form returns a status string; filter arguments return an integer count.
pub fn (mut db DB) client_kill(filters ...string) !RedisValue {
	mut args := ['CLIENT', 'KILL']
	args << filters
	return db.cmd(...args)
}

// client_pause pauses server command processing for the given milliseconds and ALL or WRITE mode.
pub fn (mut db DB) client_pause(milliseconds i64, mode string) !string {
	return db.execute_string(['CLIENT', 'PAUSE', milliseconds.str(), mode])
}

// client_unpause resumes command processing paused by client_pause.
pub fn (mut db DB) client_unpause() !string {
	return db.execute_string(['CLIENT', 'UNPAUSE'])
}

// client_reply configures replies on a dedicated connection using ON, OFF, or SKIP.
// OFF and SKIP return without reading. Send suppressed commands directly through the transport.
// Restore ON before using command methods again, or close the dedicated connection afterward.
pub fn (mut db DB) client_reply(mode string) !string {
	upper := mode.to_upper()
	if upper !in ['ON', 'OFF', 'SKIP'] {
		return CommandError{ message: '`client_reply()`: mode must be ON, OFF, or SKIP' }
	}
	if upper == 'ON' {
		return db.execute_string(['CLIENT', 'REPLY', upper])
	}
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`client_reply()`: reply suppression cannot be queued' }
	}
	db.write_resp_array(['CLIENT', 'REPLY', upper])
	db.write_data(db.cmd_buf) or {
		db.close_uncertain_server_transport(err)
		return err
	}
	return ''
}

// client_no_touch controls whether reads update key access metadata for this connection.
pub fn (mut db DB) client_no_touch(enabled bool) !string {
	return db.execute_string(['CLIENT', 'NO-TOUCH', if enabled { 'ON' } else { 'OFF' }])
}

// hello negotiates a RESP version with optional AUTH or SETNAME arguments.
// Successful AUTH credentials are restored when reconnecting.
pub fn (mut db DB) hello(version int, options ...string) !RedisValue {
	if version !in [2, 3] {
		return CommandError{ message: '`hello()`: version must be 2 or 3' }
	}
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`hello()`: protocol negotiation cannot be queued' }
	}
	if db.closed && db.config.auto_reconnect { db.reconnect()! }
	mut args := ['HELLO', version.str()]
	args << options
	previous_version := db.version
	db.version = version
	resp := db.cmd(...args) or {
		if err is CommandError {
			db.version = previous_version
		}
		return err
	}
	db.version = version
	mut index := 0
	for index < options.len {
		match options[index].to_upper() {
			'AUTH' {
				if index + 2 < options.len {
					db.config.username = options[index + 1]
					db.config.password = options[index + 2]
				}
				index += 3
			}
			'SETNAME' { index += 2 }
			else { index++ }
		}
	}
	return resp
}

// role returns the server's replication role and related metadata.
pub fn (mut db DB) role() !RedisValue {
	return db.cmd('ROLE')
}

// replicaof configures replication, using host NO and port ONE to stop replication.
pub fn (mut db DB) replicaof(host string, port string) !string {
	return db.execute_string(['REPLICAOF', host, port])
}

// slaveof configures replication using the legacy command; prefer replicaof.
pub fn (mut db DB) slaveof(host string, port string) !string {
	return db.execute_string(['SLAVEOF', host, port])
}

// dbsize returns the number of keys in the selected database.
pub fn (mut db DB) dbsize() !i64 {
	return db.execute_i64(['DBSIZE'])
}

// server_time retrieves Redis's current Unix time with microsecond precision.
pub fn (mut db DB) server_time() !ServerTime {
	resp := db.cmd('TIME')!
	if db.pipeline_mode || db.transaction_mode {
		return ServerTime{}
	}
	values := string_values(resp, 'server_time')!
	if values.len != 2 {
		return ProtocolError{ message: '`server_time()`: invalid time response' }
	}
	return ServerTime{ seconds: values[0].i64(), microseconds: values[1].i64() }
}

// flushall removes all keys from all databases, optionally in the background.
pub fn (mut db DB) flushall(asynchronous bool) !string {
	return db.execute_string(['FLUSHALL', if asynchronous { 'ASYNC' } else { 'SYNC' }])
}

// flushdb removes all keys from the selected database, optionally in the background.
pub fn (mut db DB) flushdb(asynchronous bool) !string {
	return db.execute_string(['FLUSHDB', if asynchronous { 'ASYNC' } else { 'SYNC' }])
}

// save synchronously saves a snapshot of the database.
pub fn (mut db DB) save() !string {
	return db.execute_string(['SAVE'])
}

// bgsave starts a background snapshot, optionally scheduling it after an AOF rewrite.
pub fn (mut db DB) bgsave(schedule bool) !string {
	mut args := ['BGSAVE']
	if schedule {
		args << 'SCHEDULE'
	}
	return db.execute_string(args)
}

// bgrewriteaof starts a background rewrite of the append-only file.
pub fn (mut db DB) bgrewriteaof() !string {
	return db.execute_string(['BGREWRITEAOF'])
}

// lastsave retrieves the Unix time of the last successful database snapshot.
pub fn (mut db DB) lastsave() !i64 {
	return db.execute_i64(['LASTSAVE'])
}

// monitor starts a command monitoring session on a dedicated connection.
pub fn (mut db DB) monitor() !string {
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`monitor()`: monitoring cannot be queued' }
	}
	return db.execute_string(['MONITOR'])
}

// monitor_next reads the next event from a connection in monitoring mode.
pub fn (mut db DB) monitor_next() !string {
	response := db.read_response() or {
		db.close_uncertain_server_transport(err)
		return err
	}
	return bulk_value[string](response, 'monitor_next') or {
		db.close_uncertain_server_transport(err)
		return err
	}
}

// slowlog_get retrieves slow command records, optionally limiting their count.
pub fn (mut db DB) slowlog_get(count ...int) !RedisValue {
	mut args := ['SLOWLOG', 'GET']
	for value in count {
		args << value.str()
	}
	return db.cmd(...args)
}

// slowlog_len returns the number of slow command records.
pub fn (mut db DB) slowlog_len() !i64 {
	return db.execute_i64(['SLOWLOG', 'LEN'])
}

// slowlog_reset deletes all slow command records.
pub fn (mut db DB) slowlog_reset() !string {
	return db.execute_string(['SLOWLOG', 'RESET'])
}

// swapdb exchanges the contents of two logical databases.
pub fn (mut db DB) swapdb(first int, second int) !string {
	return db.execute_string(['SWAPDB', first.str(), second.str()])
}

// memory_usage returns a key's memory usage in bytes; a missing key returns an error.
pub fn (mut db DB) memory_usage(key string, samples ...int) !i64 {
	mut args := ['MEMORY', 'USAGE', key]
	if samples.len > 1 {
		return CommandError{ message: '`memory_usage()`: at most one sample count is allowed' }
	}
	if samples.len == 1 {
		args << 'SAMPLES'
		args << samples[0].str()
	}
	response := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return 0
	}
	match response {
		i64 { return response }
		RedisNull { return NilError{ message: '`memory_usage()`: key ${key} not found' } }
		else { return ProtocolError{ message: '`memory_usage()`: unexpected response type' } }
	}
}

// memory_stats retrieves memory usage statistics, preserving RESP2 arrays or RESP3 maps.
pub fn (mut db DB) memory_stats() !RedisValue {
	return db.cmd('MEMORY', 'STATS')
}

// memory_purge asks the allocator to release unused memory to the operating system.
pub fn (mut db DB) memory_purge() !string {
	return db.execute_string(['MEMORY', 'PURGE'])
}

// memory_malloc_stats returns allocator statistics when supported by the server's allocator.
pub fn (mut db DB) memory_malloc_stats() !string {
	return db.execute_server_text(['MEMORY', 'MALLOC-STATS'])
}

// acl_whoami returns the authenticated user's name.
pub fn (mut db DB) acl_whoami() !string {
	return db.execute_string(['ACL', 'WHOAMI'])
}

// acl_list returns ACL rules for all users.
pub fn (mut db DB) acl_list() ![]string {
	return db.execute_strings(['ACL', 'LIST'])
}

// acl_users returns the names of all configured ACL users.
pub fn (mut db DB) acl_users() ![]string {
	return db.execute_strings(['ACL', 'USERS'])
}

// acl_cat lists ACL categories or commands in a specified category.
pub fn (mut db DB) acl_cat(category ...string) ![]string {
	mut args := ['ACL', 'CAT']
	args << category
	return db.execute_strings(args)
}

// acl_setuser creates or modifies an ACL user using Redis ACL rule strings.
pub fn (mut db DB) acl_setuser(user string, rules ...string) !string {
	mut args := ['ACL', 'SETUSER', user]
	args << rules
	return db.execute_string(args)
}

// acl_deluser deletes ACL users and returns the number removed.
pub fn (mut db DB) acl_deluser(users ...string) !i64 {
	mut args := ['ACL', 'DELUSER']
	args << users
	return db.execute_i64(args)
}

// acl_getuser retrieves ACL metadata, or RedisNull for a missing user.
pub fn (mut db DB) acl_getuser(user string) !RedisValue {
	return db.cmd('ACL', 'GETUSER', user)
}

// acl_log reads ACL security events or resets the log when passed RESET.
pub fn (mut db DB) acl_log(options ...string) !RedisValue {
	mut args := ['ACL', 'LOG']
	args << options
	return db.cmd(...args)
}

// acl_dryrun checks whether a user may execute a command without executing it.
pub fn (mut db DB) acl_dryrun(user string, command ...string) !string {
	mut args := ['ACL', 'DRYRUN', user]
	args << command
	return db.execute_string(args)
}

// acl_genpass generates a secure password, optionally selecting its number of random bits.
pub fn (mut db DB) acl_genpass(bits ...int) !string {
	mut args := ['ACL', 'GENPASS']
	for value in bits {
		args << value.str()
	}
	return db.execute_string(args)
}

// acl_save saves configured ACL users to the server's ACL file.
pub fn (mut db DB) acl_save() !string {
	return db.execute_string(['ACL', 'SAVE'])
}

// acl_load reloads ACL users from the server's ACL file.
pub fn (mut db DB) acl_load() !string {
	return db.execute_string(['ACL', 'LOAD'])
}

// quit closes the server connection after replying OK.
pub fn (mut db DB) quit() !string {
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`quit()`: connection closure cannot be queued' }
	}
	result := db.execute_string(['QUIT'])!
	db.close()!
	return result
}

// shutdown stops Redis using optional SAVE, NOSAVE, NOW, FORCE, or ABORT arguments.
// A clean close before any response prefix confirms shutdown; incomplete replies are errors.
pub fn (mut db DB) shutdown(options ...string) ! {
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`shutdown()`: server shutdown cannot be queued' }
	}
	mut args := ['SHUTDOWN']
	args << options
	db.write_resp_array(args)
	db.write_data(db.cmd_buf) or {
		db.close_uncertain_server_transport(err)
		return err
	}
	prefix := db.read_response_prefix() or {
		if err is ConnectionError && err.eof {
			db.close()!
			return
		}
		db.close_uncertain_server_transport(err)
		return err
	}
	response := db.read_response_payload(prefix, false) or {
		db.close_uncertain_server_transport(err)
		return err
	}
	if response is string && response == 'OK' {
		return
	}
	db.close() or {}
	return ProtocolError{ message: '`shutdown()`: unexpected response type' }
}

// command returns metadata for all Redis commands.
pub fn (mut db DB) command() !RedisValue {
	return db.cmd('COMMAND')
}

// command_info returns metadata for the named Redis commands.
pub fn (mut db DB) command_info(commands ...string) !RedisValue {
	mut args := ['COMMAND', 'INFO']
	args << commands
	return db.cmd(...args)
}

// command_count returns the number of commands supported by Redis.
pub fn (mut db DB) command_count() !i64 {
	return db.execute_i64(['COMMAND', 'COUNT'])
}

// command_getkeys extracts key arguments from a command without executing it.
pub fn (mut db DB) command_getkeys(command ...string) ![]string {
	mut args := ['COMMAND', 'GETKEYS']
	args << command
	return db.execute_strings(args)
}

// cluster_info returns cluster state and statistics.
pub fn (mut db DB) cluster_info() !string {
	return db.execute_server_text(['CLUSTER', 'INFO'])
}

// cluster_help returns descriptions of Redis Cluster subcommands.
pub fn (mut db DB) cluster_help() ![]string {
	return db.execute_strings(['CLUSTER', 'HELP'])
}

// cluster_nodes returns the cluster's node configuration.
pub fn (mut db DB) cluster_nodes() !string {
	return db.execute_server_text(['CLUSTER', 'NODES'])
}

// cluster_myid returns the current node's cluster identifier.
pub fn (mut db DB) cluster_myid() !string {
	return db.execute_string(['CLUSTER', 'MYID'])
}

// cluster_slots returns cluster slot ownership metadata.
pub fn (mut db DB) cluster_slots() !RedisValue {
	return db.cmd('CLUSTER', 'SLOTS')
}

// cluster_keyslot returns the hash slot assigned to a key by Redis Cluster.
pub fn (mut db DB) cluster_keyslot(key string) !i64 {
	return db.execute_i64(['CLUSTER', 'KEYSLOT', key])
}

// cluster_countkeysinslot counts keys assigned to a slot on this cluster node.
pub fn (mut db DB) cluster_countkeysinslot(slot int) !i64 {
	return db.execute_i64(['CLUSTER', 'COUNTKEYSINSLOT', slot.str()])
}

// cluster_getkeysinslot retrieves up to count keys assigned to a slot on this node.
pub fn (mut db DB) cluster_getkeysinslot(slot int, count int) ![]string {
	return db.execute_strings(['CLUSTER', 'GETKEYSINSLOT', slot.str(), count.str()])
}
