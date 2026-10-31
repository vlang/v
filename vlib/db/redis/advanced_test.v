// vtest build: started_redis?
module redis

import os
import rand
import time

fn advanced_connection(version int) !DB {
	mut db := connect(password: os.getenv('VREDIS_PASSWORD'))!
	db.cmd('HELLO', version.str())!
	db.version = version
	return db
}

fn test_transactions_and_optimistic_locking() {
	for version in [2, 3] {
		mut db := advanced_connection(version)!
		mut other := advanced_connection(version)!
		key := 'advanced:transaction:${rand.uuid_v4()}'
		defer {
			db.del(key) or {}
			db.close() or {}
			other.close() or {}
		}
		assert db.multi()! == 'OK'
		assert db.set(key, '1')! == ''
		assert db.incr(key)! == 0
		assert db.get[string](key)! == ''
		values := db.exec()!
		assert values.len == 3
		assert values[0] as string == 'OK'
		assert values[1] as i64 == 2
		assert (values[2] as []u8).bytestr() == '2'
		assert db.multi()! == 'OK'
		db.set(key, 'discarded')!
		assert db.discard()! == 'OK'
		assert db.get[string](key)! == '2'
		assert db.watch(key)! == 'OK'
		assert other.set(key, 'changed')! == 'OK'
		assert db.multi()! == 'OK'
		db.set(key, 'lost update')!
		mut aborted := false
		db.exec() or {
			aborted = true
			assert err is NilError
			assert db.get[string](key)! == 'changed'
		}
		assert aborted
		assert db.unwatch()! == 'OK'
		db.transaction_start()!
		db.set(key, 7)!
		db.incr(key)!
		buffered := db.transaction_execute()!
		assert buffered.len == 2
		assert buffered[1] as i64 == 8
		assert db.get[int](key)! == 8
		// A pipeline started after MULTI has already put the server into transaction mode.
		db.multi()!
		db.pipeline_start()
		db.set(key, 99)!
		db.reset()!
		assert db.get[int](key)! == 8
		db.multi()!
		db.pipeline_start()
		assert db.transaction_execute()!.len == 0
		db.watch(key)!
		db.transaction_start()!
		db.set(key, 99)!
		db.reset()!
		assert db.get[int](key)! == 8
		assert !db.watched
		db.transaction_start()!
		db.set(key, 999)!
		assert db.discard()! == 'OK'
		assert db.get[int](key)! == 8
		// Runtime errors are retained inside EXEC while the following replies remain readable.
		db.multi()!
		db.cmd('LPUSH', key, 'wrong type')!
		db.get[int](key)!
		error_values := db.exec()!
		assert error_values.len == 2
		assert error_values[0] is RedisBlobError
		assert (error_values[1] as []u8).bytestr() == '8'
		assert db.ping()! == 'PONG'
		// Queue-time errors abort a buffered transaction, without leaving unread replies.
		db.transaction_start()!
		db.cmd('SET', key)!
		db.get[int](key)!
		mut queue_error := false
		db.transaction_execute() or {
			queue_error = true
			assert err is CommandError
		}
		assert queue_error
		assert db.ping()! == 'PONG'
	}
}

fn advanced_delayed_publish(channel string) {
	time.sleep(100 * time.millisecond)
	mut publisher := connect(password: os.getenv('VREDIS_PASSWORD')) or { panic(err) }
	defer { publisher.close() or {} }
	publisher.publish(channel, 'callback') or { panic(err) }
}

fn test_dedicated_pubsub_binary_and_callbacks() {
	for version in [2, 3] {
		mut publisher := advanced_connection(version)!
		mut sub := connect_pubsub(
			password: os.getenv('VREDIS_PASSWORD')
		)!
		sub.db.cmd('HELLO', version.str())!
		sub.db.version = version
		defer {
			publisher.close() or {}
			sub.close() or {}
		}
		channel := 'advanced:pubsub:${rand.uuid_v4()}'
		other_channel := '${channel}:other'
		payload := [u8(0), 13, 10, 255]
		sub.subscribe(channel, other_channel)!
		assert publisher.publish(channel, payload)! == 1
		// A message can arrive before a later subscription acknowledgment.
		sub.psubscribe('${channel}*')!
		message := sub.next_message()!
		assert message.channel == channel
		assert message.pattern == ''
		assert message.payload == payload
		assert publisher.publish(other_channel, 'pattern')! == 2
		first := sub.next_message()!
		second := sub.next_message()!
		assert first.channel == other_channel
		assert second.channel == other_channel
		assert first.payload.bytestr() == 'pattern'
		assert second.payload.bytestr() == 'pattern'
		assert first.pattern != second.pattern
		sub.unsubscribe()!
		assert publisher.publish(channel, 'pattern only')! == 1
		pattern_message := sub.next_message()!
		assert pattern_message.pattern == '${channel}*'
		sub.punsubscribe()!
		assert publisher.publish(channel, 'nobody')! == 0
		spawn advanced_delayed_publish(channel)
		sub.subscribe_with_callback([channel], fn (msg PubSubMessage) bool {
			assert msg.payload.bytestr() == 'callback'
			return false
		})!
		sub.unsubscribe(channel)!
		mut unsubscribed := false
		sub.next_message() or {
			unsubscribed = true
			assert err.msg().contains('no active subscriptions')
		}
		assert unsubscribed
	}
}

fn test_stream_records_groups_claims_and_metadata() {
	for version in [2, 3] {
		mut db := advanced_connection(version)!
		key := 'advanced:stream:${rand.uuid_v4()}'
		other := '${key}:other'
		defer {
			db.unlink(key, other) or {}
			db.close() or {}
		}
		binary := [u8(0), 13, 10, 255].bytestr()
		assert db.xadd(key, '1-0', {
			'field': binary
		})! == '1-0'
		assert db.xadd(key, '2-0', {
			'field': 'two'
		})! == '2-0'
		assert db.xadd(key, '3-0', {
			'field': 'three'
		})! == '3-0'
		assert db.xadd(other, '1-0', {
			'number': 42
		})! == '1-0'
		assert db.xadd(other, '2-0', {
			'number': 43
		}, trim: StreamTrim{ maxlen: 1 })! == '2-0'
		assert db.xlen(other)! == 1
		mut missing_stream := false
		db.xadd('${key}:missing', '*', {
			'field': 'value'
		}, nomkstream: true) or {
			missing_stream = true
			assert err is NilError
		}
		assert missing_stream
		assert db.xlen(key)! == 3
		range := db.xrange(key, '-', '+')!
		assert range.len == 3
		assert range[0].fields['field'] == binary
		assert !range[0].deleted
		reverse := db.xrevrange(key, '+', '-', count: 1)!
		assert reverse.len == 1
		assert reverse[0].id == '3-0'
		reads := db.xread([StreamOffset{ key: key, id: '0' }, StreamOffset{ key: other, id: '0' }])!
		assert reads.len == 2
		for read in reads {
			if read.key == key {
				assert read.entries.len == 3
			} else {
				assert read.entries[0].fields['number'] == '43'
			}
		}
		assert db.xread([StreamOffset{ key: key, id: '3-0' }], block: 1)!.len == 0
		assert db.xgroup_create(key, 'workers', '0')! == 'OK'
		assert db.xgroup_createconsumer(key, 'workers', 'one')!
		assert !db.xgroup_createconsumer(key, 'workers', 'one')!
		group_reads := db.xreadgroup('workers', 'one', [StreamOffset{ key: key, id: '>' }],
			count: 2
		)!
		assert group_reads.len == 1
		assert group_reads[0].entries.len == 2
		pending := db.xpending(key, 'workers')!
		assert pending.count == 2
		assert pending.smallest or { '' } == '1-0'
		assert pending.largest or { '' } == '2-0'
		assert pending.consumers[0].consumer == 'one'
		pending_entries := db.xpending_range(key, 'workers', '-', '+', 10, consumer: 'one', idle: 0)!
		assert pending_entries.len == 2
		assert pending_entries[0].deliveries == 1
		claimed := db.xclaim(key, 'workers', 'two', 0, ['1-0'])!
		assert claimed.len == 1
		assert claimed[0].fields['field'] == binary
		assert db.xclaim_ids(key, 'workers', 'two', 0, ['2-0'])! == ['2-0']
		autoclaimed := db.xautoclaim(key, 'workers', 'three', 0, '0-0')!
		assert autoclaimed.entries.len == 2
		assert autoclaimed.cursor == '0-0'
		ids_only := db.xautoclaim(key, 'workers', 'four', 0, '0-0', justid: true)!
		assert ids_only.ids == ['1-0', '2-0']
		stream_info := db.xinfo_stream(key)!
		assert (stream_info['length'] or { panic('missing length') }) as i64 == 3
		full_info := db.xinfo_stream(key, full: true, count: 2)!
		assert (full_info['length'] or { panic('missing length') }) as i64 == 3
		assert db.xinfo_groups(key)!.len == 1
		assert db.xinfo_consumers(key, 'workers')!.len == 4
		assert db.xack(key, 'workers', '1-0', '2-0')! == 2
		assert db.xpending(key, 'workers')!.count == 0
		assert db.xgroup_setid(key, 'workers', '0')! == 'OK'
		db.xreadgroup('workers', 'one', [StreamOffset{ key: key, id: '>' }], count: 1)!
		assert db.xdel(key, '1-0')! == 1
		deleted := db.xreadgroup('workers', 'one', [StreamOffset{ key: key, id: '0' }])!
		assert deleted[0].entries[0].deleted
		deleted_claim := db.xautoclaim(key, 'workers', 'two', 0, '0-0')!
		assert deleted_claim.deleted_ids == ['1-0']
		assert db.xgroup_delconsumer(key, 'workers', 'one')! == 0
		assert db.xgroup_destroy(key, 'workers')!
		assert !db.xgroup_destroy(key, 'workers')!
		assert db.xtrim(key, maxlen: 1)! == 1
		assert db.xlen(key)! == 1
		assert db.xtrim(key, minid: '4-0')! == 1
		assert db.xlen(key)! == 0
		assert db.xgroup_create(other, 'empty', '$', mkstream: true)! == 'OK'
	}
}
