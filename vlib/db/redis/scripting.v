module redis

fn script_call_args(command string, script string, keys []string, arguments []string) []string {
	mut args := [command, script, keys.len.str()]
	args << keys
	args << arguments
	return args
}

// eval executes a Lua script with explicit keys and arguments and preserves its response shape.
pub fn (mut db DB) eval(script string, keys []string, arguments ...string) !RedisValue {
	return db.cmd(...script_call_args('EVAL', script, keys, arguments))
}

// eval_ro executes a read-only Lua script with explicit keys and arguments.
pub fn (mut db DB) eval_ro(script string, keys []string, arguments ...string) !RedisValue {
	return db.cmd(...script_call_args('EVAL_RO', script, keys, arguments))
}

// evalsha executes a previously loaded Lua script by SHA1 digest.
pub fn (mut db DB) evalsha(sha string, keys []string, arguments ...string) !RedisValue {
	return db.cmd(...script_call_args('EVALSHA', sha, keys, arguments))
}

// evalsha_ro executes a previously loaded read-only script by SHA1 digest.
pub fn (mut db DB) evalsha_ro(sha string, keys []string, arguments ...string) !RedisValue {
	return db.cmd(...script_call_args('EVALSHA_RO', sha, keys, arguments))
}

// script_load loads a Lua script into Redis's script cache and returns its SHA1 digest.
pub fn (mut db DB) script_load(script string) !string {
	return db.execute_string(['SCRIPT', 'LOAD', script])
}

// script_exists checks script cache membership in digest order.
pub fn (mut db DB) script_exists(hashes ...string) ![]bool {
	mut args := ['SCRIPT', 'EXISTS']
	args << hashes
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []bool{}
	}
	values := array_value(resp, 'script_exists')!
	mut result := []bool{cap: values.len}
	for value in values {
		if value is i64 {
			result << value != 0
		} else {
			return ProtocolError{ message: '`script_exists()`: unexpected response type' }
		}
	}
	return result
}

// script_flush removes all cached scripts; asynchronous selects Redis's ASYNC mode.
pub fn (mut db DB) script_flush(asynchronous bool) !string {
	return db.execute_string(['SCRIPT', 'FLUSH', if asynchronous { 'ASYNC' } else { 'SYNC' }])
}

// script_kill interrupts a running script that has not modified the dataset.
pub fn (mut db DB) script_kill() !string {
	return db.execute_string(['SCRIPT', 'KILL'])
}

// script_debug selects Redis's YES, SYNC, or NO Lua debugger mode.
pub fn (mut db DB) script_debug(mode string) !string {
	return db.execute_string(['SCRIPT', 'DEBUG', mode])
}

// fcall invokes a registered Redis function with explicit keys and arguments.
pub fn (mut db DB) fcall(function string, keys []string, arguments ...string) !RedisValue {
	return db.cmd(...script_call_args('FCALL', function, keys, arguments))
}

// fcall_ro invokes a registered read-only Redis function.
pub fn (mut db DB) fcall_ro(function string, keys []string, arguments ...string) !RedisValue {
	return db.cmd(...script_call_args('FCALL_RO', function, keys, arguments))
}

// function_load loads a Redis function library; replace allows replacing an existing library.
pub fn (mut db DB) function_load(code string, replace bool) !string {
	mut args := ['FUNCTION', 'LOAD']
	if replace {
		args << 'REPLACE'
	}
	args << code
	return db.execute_string(args)
}

// function_delete removes a Redis function library.
pub fn (mut db DB) function_delete(library string) !string {
	return db.execute_string(['FUNCTION', 'DELETE', library])
}

// function_list returns function library metadata, with Redis's optional filter and code arguments.
pub fn (mut db DB) function_list(options ...string) !RedisValue {
	mut args := ['FUNCTION', 'LIST']
	args << options
	return db.cmd(...args)
}

// function_stats returns metadata about the function runtime and executing function.
pub fn (mut db DB) function_stats() !RedisValue {
	return db.cmd('FUNCTION', 'STATS')
}

// function_dump serializes all registered function libraries for function_restore.
pub fn (mut db DB) function_dump() ![]u8 {
	return db.execute_bulk[[]u8](['FUNCTION', 'DUMP'])
}

// function_restore restores serialized function libraries using APPEND, FLUSH, or REPLACE.
pub fn (mut db DB) function_restore(data []u8, policy string) !string {
	mut args := ['FUNCTION', 'RESTORE', data.bytestr()]
	if policy.len > 0 {
		args << policy
	}
	return db.execute_string(args)
}

// function_flush removes all function libraries; asynchronous selects Redis's ASYNC mode.
pub fn (mut db DB) function_flush(asynchronous bool) !string {
	return db.execute_string(['FUNCTION', 'FLUSH', if asynchronous { 'ASYNC' } else { 'SYNC' }])
}

// function_kill interrupts a running function that has not modified the dataset.
pub fn (mut db DB) function_kill() !string {
	return db.execute_string(['FUNCTION', 'KILL'])
}
