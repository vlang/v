module redis

// multi starts a transaction. Typed commands return placeholders until exec supplies their results.
pub fn (mut db DB) multi() !string {
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`multi()`: a pipeline or transaction is already active' }
	}
	result := db.execute_string(['MULTI'])!
	db.transaction_mode = true
	db.buffered_transaction = false
	return result
}

fn transaction_result(resp RedisValue) ![]RedisValue {
	if resp is RedisNull {
		return NilError{ message: 'transaction aborted because a watched key changed' }
	}
	if resp is RedisBlobError {
		return CommandError{ message: resp.data.bytestr() }
	}
	return array_value(resp, 'exec')
}

// exec executes queued transaction commands. Aborted WATCH transactions return NilError.
// Individual command errors remain RedisBlobError values in the returned result array.
pub fn (mut db DB) exec() ![]RedisValue {
	if db.pipeline_mode {
		return CommandError{ message: '`exec()`: use transaction_execute for a buffered transaction' }
	}
	defer {
		db.transaction_mode = false
		db.watched = false
		db.buffered_transaction = false
	}
	return transaction_result(db.cmd('EXEC')!)
}

// discard cancels a transaction, including a transaction buffered by transaction_start.
pub fn (mut db DB) discard() !string {
	if db.pipeline_mode && db.transaction_mode {
		buffered := db.buffered_transaction
		db.pipeline_mode = false
		db.pipeline_buffer.clear()
		db.pipeline_cmd_count = 0
		if buffered {
			db.transaction_mode = false
			db.buffered_transaction = false
			// MULTI was buffered locally, but WATCH may already have reached the server.
			return db.unwatch()
		}
	}
	if db.pipeline_mode {
		return CommandError{ message: '`discard()`: cannot discard a normal pipeline' }
	}
	defer {
		db.transaction_mode = false
		db.watched = false
		db.buffered_transaction = false
	}
	return bulk_value[string](db.cmd('DISCARD')!, 'discard')
}

// watch makes exec abort if any watched key changes before the transaction executes.
pub fn (mut db DB) watch(keys ...string) !string {
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`watch()`: watch keys before starting a transaction' }
	}
	mut args := ['WATCH']
	args << keys
	result := db.execute_string(args)!
	db.watched = true
	return result
}

// unwatch removes all watched keys from this connection.
pub fn (mut db DB) unwatch() !string {
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`unwatch()`: cannot unwatch inside a pipeline or transaction' }
	}
	result := db.execute_string(['UNWATCH'])!
	db.watched = false
	return result
}

// transaction_start buffers MULTI and subsequent commands for execution in one write.
// Call watch before this method when optimistic locking is needed.
pub fn (mut db DB) transaction_start() ! {
	if db.pipeline_mode || db.transaction_mode {
		return CommandError{ message: '`transaction_start()`: a pipeline or transaction is already active' }
	}
	db.pipeline_start()
	db.cmd('MULTI')!
	db.transaction_mode = true
	db.buffered_transaction = true
}

// transaction_execute sends a buffered transaction and returns its EXEC results.
// It consumes all intermediate replies, preserving synchronization after command errors.
pub fn (mut db DB) transaction_execute() ![]RedisValue {
	if !db.pipeline_mode || !db.transaction_mode {
		return CommandError{ message: '`transaction_execute()`: buffered transaction not started' }
	}
	db.cmd('EXEC')!
	buffered := db.buffered_transaction
	defer {
		db.transaction_mode = false
		db.watched = false
		db.buffered_transaction = false
	}
	results := db.pipeline_execute()!
	if results.len < 1 || (buffered && results.len < 2) {
		return ProtocolError{ message: '`transaction_execute()`: invalid transaction response' }
	}
	return transaction_result(results[results.len - 1])
}
