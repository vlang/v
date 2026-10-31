module redis

import time

// Redis is the common command interface implemented by DB and mocks.
// Typed helpers remain on DB; cmd provides access to every Redis command.
pub interface Redis {
mut:
	cmd(...string) !RedisValue
	ping() !string
	close() !
	validate() !bool
	reset() !
}

// Metrics records completed commands, failures, reconnects, and total command time.
pub struct Metrics {
pub mut:
	commands   u64
	failures   u64
	reconnects u64
	duration   time.Duration
}

// CommandTrace describes a completed command without including keys, values, or credentials.
pub struct CommandTrace {
pub:
	command  string
	duration time.Duration
	failed   bool
}

fn (mut db DB) record_command(command string, started time.Time, failed bool) {
	elapsed := time.now() - started
	db.metrics.commands++
	db.metrics.duration += elapsed
	if failed { db.metrics.failures++ }
	if hook := db.config.trace_hook {
		hook(CommandTrace{ command: command.to_upper(), duration: elapsed, failed: failed })
	}
}

fn (mut db DB) record_pipeline(replies []RedisValue, started time.Time) {
	elapsed := time.now() - started
	db.metrics.commands += u64(replies.len)
	db.metrics.duration += elapsed
	mut failed := false
	for reply in replies {
		if reply is RedisBlobError {
			db.metrics.failures++
			failed = true
		}
	}
	if hook := db.config.trace_hook {
		hook(CommandTrace{ command: 'PIPELINE', duration: elapsed, failed: failed })
	}
}

// statistics returns the connection's command and reconnect counters.
pub fn (db DB) statistics() Metrics {
	return db.metrics
}

// reconnect replaces the transport and restores authentication and the selected database.
// Pending pipelines and transactions cannot be replayed and must be discarded explicitly.
pub fn (mut db DB) reconnect() ! {
	if db.pipeline_mode || db.transaction_mode || db.watched {
		return ConnectionError{ message: 'cannot reconnect during a pipeline or transaction' }
	}
	config := db.config
	mut metrics := db.metrics
	db.close() or {}
	db = connect(config)!
	metrics.reconnects++
	db.metrics = metrics
}

// select_db selects a logical database and remembers it for reconnects.
pub fn (mut db DB) select_db(database int) !string {
	if database < 0 || db.pipeline_mode || db.transaction_mode || db.watched {
		return CommandError{ message: 'select_db requires a nonnegative database outside a pipeline or transaction' }
	}
	result := db.execute_string(['SELECT', database.str()])!
	db.config.database = database
	return result
}

// AsyncResult contains a command reply or the original error.
pub struct AsyncResult {
pub:
	value RedisValue = RedisNull{}
	err   ?IError
}

// AsyncClient runs independent commands on separate connections using V coroutines.
// Each request owns its connection; transaction and pipeline state is never shared.
pub struct AsyncClient {
pub:
	config Config
}

// async_client creates a client for independent concurrent commands.
pub fn async_client(config Config) AsyncClient {
	return AsyncClient{ config: config }
}

// cmd_async starts a command and returns a channel carrying exactly one result.
pub fn (client AsyncClient) cmd_async(args ...string) chan AsyncResult {
	result := chan AsyncResult{cap: 1}
	spawn async_command(client.config, args.clone(), result)
	return result
}

fn async_command(config Config, args []string, result chan AsyncResult) {
	mut db := connect(config) or {
		result <- AsyncResult{ err: err }
		return
	}
	defer { db.close() or {} }
	reply := db.cmd(...args) or {
		result <- AsyncResult{ err: err }
		return
	}
	result <- AsyncResult{ value: reply }
}
