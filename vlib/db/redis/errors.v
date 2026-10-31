module redis

// RedisError identifies errors returned by the Redis client.
pub interface RedisError {
	msg() string
	code() int
}

// ConnectionError represents a connection or transport failure.
pub struct ConnectionError {
pub:
	message string
	eof     bool // true only when the peer closed the transport cleanly
}

// msg returns the error message.
pub fn (e ConnectionError) msg() string {
	return e.message
}

// code returns the Redis client error category.
pub fn (e ConnectionError) code() int {
	return 1
}

// CommandError represents a Redis command error reply.
pub struct CommandError {
pub:
	message string
}

// msg returns the error message.
pub fn (e CommandError) msg() string {
	return e.message
}

// code returns the Redis client error category.
pub fn (e CommandError) code() int {
	return 2
}

// ProtocolError represents an invalid RESP frame or reply shape.
pub struct ProtocolError {
pub:
	message string
}

// msg returns the error message.
pub fn (e ProtocolError) msg() string {
	return e.message
}

// code returns the Redis client error category.
pub fn (e ProtocolError) code() int {
	return 3
}

// AuthError represents an authentication failure.
pub struct AuthError {
pub:
	message string
}

// msg returns the error message.
pub fn (e AuthError) msg() string {
	return e.message
}

// code returns the Redis client error category.
pub fn (e AuthError) code() int {
	return 4
}

// NilError represents a missing value or aborted transaction.
pub struct NilError {
pub:
	message string
}

// msg returns the error message.
pub fn (e NilError) msg() string {
	return e.message
}

// code returns the Redis client error category.
pub fn (e NilError) code() int {
	return 5
}
