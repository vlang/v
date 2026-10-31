module redis

// PubSubMessage is a binary-safe published message. Pattern is set for pattern subscriptions.
pub struct PubSubMessage {
pub:
	channel string
	pattern string
	payload []u8
}

// PubSub owns a dedicated connection. Use a separate DB for commands and publishing.
// Its methods must not be called concurrently on the same connection.
pub struct PubSub {
mut:
	db       DB
	channels []string
	patterns []string
	pending  []PubSubMessage
}

// connect_pubsub opens a dedicated connection with the same configuration as connect.
pub fn connect_pubsub(config Config) !PubSub {
	return PubSub{ db: connect(config)! }
}

// close terminates the dedicated subscription connection.
pub fn (mut sub PubSub) close() ! {
	sub.db.close()!
}

fn pubsub_values(resp RedisValue) ![]RedisValue {
	match resp {
		RedisPush { return resp.elements }
		[]RedisValue { return resp }
		else { return ProtocolError{ message: 'unexpected Pub/Sub response type' } }
	}
}

fn (mut sub PubSub) consume_frame(values []RedisValue) !(bool, PubSubMessage) {
	if values.len < 3 {
		return ProtocolError{ message: 'invalid Pub/Sub response' }
	}
	kind := bulk_value[string](values[0], 'pubsub')!
	match kind {
		'message' {
			if values.len != 3 {
				return ProtocolError{ message: 'invalid Pub/Sub message' }
			}
			return true, PubSubMessage{
				channel: bulk_value[string](values[1], 'pubsub')!
				payload: bulk_value[[]u8](values[2], 'pubsub')!
			}
		}
		'pmessage' {
			if values.len != 4 {
				return ProtocolError{ message: 'invalid Pub/Sub pattern message' }
			}
			return true, PubSubMessage{
				pattern: bulk_value[string](values[1], 'pubsub')!
				channel: bulk_value[string](values[2], 'pubsub')!
				payload: bulk_value[[]u8](values[3], 'pubsub')!
			}
		}
		'subscribe', 'psubscribe', 'unsubscribe', 'punsubscribe' {
			if values.len != 3 || values[2] !is i64 {
				return ProtocolError{ message: 'invalid Pub/Sub subscription acknowledgment' }
			}
			if values[1] !is RedisNull {
				name := bulk_value[string](values[1], 'pubsub')!
				match kind {
					'subscribe' {
						if name !in sub.channels { sub.channels << name }
					}
					'psubscribe' {
						if name !in sub.patterns { sub.patterns << name }
					}
					'unsubscribe' { sub.channels = sub.channels.filter(it != name) }
					else { sub.patterns = sub.patterns.filter(it != name) }
				}
			}
			return false, PubSubMessage{}
		}
		else { return ProtocolError{ message: 'unexpected Pub/Sub response: ${kind}' } }
	}
}

fn (mut sub PubSub) subscription_command(command string, names []string, acknowledgments int) ! {
	mut args := [command]
	args << names
	sub.db.write_resp_array(args)
	sub.db.write_data(sub.db.cmd_buf) or {
		sub.close() or {}
		return err
	}
	mut acknowledged := 0
	for acknowledged < acknowledgments {
		kind, has_message, message := sub.read_frame()!
		if has_message {
			sub.pending << message
		} else {
			if kind != command.to_lower() {
				sub.close() or {}
				return ProtocolError{ message: 'unexpected Pub/Sub subscription acknowledgment' }
			}
			acknowledged++
		}
	}
}

fn (mut sub PubSub) read_frame() !(string, bool, PubSubMessage) {
	resp := sub.db.read_response() or {
		sub.close() or {}
		return err
	}
	values := pubsub_values(resp) or {
		sub.close() or {}
		return err
	}
	has_message, message := sub.consume_frame(values) or {
		sub.close() or {}
		return err
	}
	return bulk_value[string](values[0], 'pubsub')!, has_message, message
}

// subscribe subscribes to channels and consumes their acknowledgments before returning.
pub fn (mut sub PubSub) subscribe(channels ...string) ! {
	if channels.len == 0 {
		return CommandError{ message: '`subscribe()`: at least one channel is required' }
	}
	sub.subscription_command('SUBSCRIBE', channels, channels.len)!
}

// psubscribe subscribes to glob patterns and consumes their acknowledgments before returning.
pub fn (mut sub PubSub) psubscribe(patterns ...string) ! {
	if patterns.len == 0 {
		return CommandError{ message: '`psubscribe()`: at least one pattern is required' }
	}
	sub.subscription_command('PSUBSCRIBE', patterns, patterns.len)!
}

// unsubscribe removes channel subscriptions. With no arguments, it removes all channels.
pub fn (mut sub PubSub) unsubscribe(channels ...string) ! {
	count := if channels.len > 0 {
		channels.len
	} else {
		if sub.channels.len > 0 { sub.channels.len } else { 1 }
	}
	sub.subscription_command('UNSUBSCRIBE', channels, count)!
}

// punsubscribe removes pattern subscriptions. With no arguments, it removes all patterns.
pub fn (mut sub PubSub) punsubscribe(patterns ...string) ! {
	count := if patterns.len > 0 {
		patterns.len
	} else {
		if sub.patterns.len > 0 { sub.patterns.len } else { 1 }
	}
	sub.subscription_command('PUNSUBSCRIBE', patterns, count)!
}

// next_message waits for a published message, skipping subscription acknowledgments.
// The configured read timeout also applies to this blocking call.
pub fn (mut sub PubSub) next_message() !PubSubMessage {
	if sub.pending.len > 0 {
		message := sub.pending[0]
		sub.pending.delete(0)
		return message
	}
	if sub.channels.len == 0 && sub.patterns.len == 0 {
		return CommandError{ message: '`next_message()`: no active subscriptions' }
	}
	for {
		_, has_message, message := sub.read_frame()!
		if has_message {
			return message
		}
	}
	return ProtocolError{ message: 'unreachable Pub/Sub state' }
}

// listen invokes callback for each message until callback returns false.
pub fn (mut sub PubSub) listen(callback fn (PubSubMessage) bool) ! {
	for {
		if !callback(sub.next_message()!) {
			return
		}
	}
}

// subscribe_with_callback subscribes to channels and processes messages until callback returns false.
pub fn (mut sub PubSub) subscribe_with_callback(channels []string, callback fn (PubSubMessage) bool) ! {
	sub.subscribe(...channels)!
	sub.listen(callback)!
}

// psubscribe_with_callback subscribes to patterns and processes messages until callback returns false.
pub fn (mut sub PubSub) psubscribe_with_callback(patterns []string, callback fn (PubSubMessage) bool) ! {
	sub.psubscribe(...patterns)!
	sub.listen(callback)!
}

// publish sends a message to subscribers and returns the number of receiving subscriptions.
pub fn (mut db DB) publish[T](channel string, message T) !i64 {
	return db.execute_i64(['PUBLISH', channel, value_string(message, 'publish')!])
}
