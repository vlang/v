// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import time

// 0-RTT (Zero Round Trip Time) Connection Resumption
// Allows clients to send data in the first flight without waiting for handshake completion

// SessionTicket represents a session ticket for 0-RTT resumption
pub struct SessionTicket {
pub mut:
	ticket          []u8
	creation_time   time.Time
	max_early_data  u32
	alpn_protocol   string
	server_name     string
	cipher_suite    u16
	ticket_lifetime u32 // seconds
}

// EarlyData represents data sent during 0-RTT
pub struct EarlyData {
pub mut:
	data      []u8
	stream_id u64
}

// SessionCache manages session tickets for 0-RTT resumption
pub struct SessionCache {
mut:
	tickets map[string]SessionTicket // key: server_name
	max_age time.Duration = 24 * time.hour
}

pub fn new_session_cache() SessionCache {
	return SessionCache{
		tickets: map[string]SessionTicket{}
	}
}

// store stores a session ticket for a server
pub fn (mut sc SessionCache) store(server_name string, ticket SessionTicket) {
	sc.tickets[server_name] = ticket
}

// get retrieves a session ticket for a server
pub fn (sc &SessionCache) get(server_name string) ?SessionTicket {
	if ticket := sc.tickets[server_name] {
		// Check if ticket is still valid
		age := time.now() - ticket.creation_time
		if age.seconds() < ticket.ticket_lifetime {
			return ticket
		}
	}
	return none
}

// remove removes a session ticket for a server
pub fn (mut sc SessionCache) remove(server_name string) {
	sc.tickets.delete(server_name)
}

// cleanup removes expired tickets
pub fn (mut sc SessionCache) cleanup() {
	now := time.now()

	// Filter in-place instead of building removal list
	mut new_tickets := map[string]SessionTicket{}
	for server_name, ticket in sc.tickets {
		age := now - ticket.creation_time
		if age.seconds() < ticket.ticket_lifetime {
			new_tickets[server_name] = ticket
		}
	}
	sc.tickets = new_tickets.move()
}

// ZeroRTTConfig configures 0-RTT behavior
pub struct ZeroRTTConfig {
pub:
	enabled        bool = true
	max_early_data u32  = 16384 // 16KB default
	anti_replay    bool = true  // Enable anti-replay protection
	max_ticket_age u32  = 86400 // 24 hours in seconds
}

// ZeroRTTState tracks the state of a 0-RTT connection attempt
pub enum ZeroRTTState {
	disabled
	attempting
	accepted
	rejected
}

// ZeroRTTConnection manages a 0-RTT connection
pub struct ZeroRTTConnection {
pub mut:
	state          ZeroRTTState
	early_data     []EarlyData
	bytes_sent     u32
	max_early_data u32
	ticket         ?SessionTicket
}

pub fn new_zero_rtt_connection(config ZeroRTTConfig) ZeroRTTConnection {
	return ZeroRTTConnection{
		state:          if config.enabled { ZeroRTTState.attempting } else { ZeroRTTState.disabled }
		early_data:     []EarlyData{}
		bytes_sent:     0
		max_early_data: config.max_early_data
		ticket:         none
	}
}

// can_send_early_data checks if more early data can be sent
pub fn (zc &ZeroRTTConnection) can_send_early_data() bool {
	return zc.state == .attempting && zc.bytes_sent < zc.max_early_data
}

// add_early_data adds data to be sent as 0-RTT
pub fn (mut zc ZeroRTTConnection) add_early_data(data []u8, stream_id u64) !bool {
	if !zc.can_send_early_data() {
		return error('Cannot send early data in current state')
	}

	if zc.bytes_sent + u32(data.len) > zc.max_early_data {
		return error('Early data size exceeds maximum')
	}

	zc.early_data << EarlyData{
		data:      data.clone()
		stream_id: stream_id
	}
	zc.bytes_sent += u32(data.len)

	return true
}

// accept marks 0-RTT as accepted by server
pub fn (mut zc ZeroRTTConnection) accept() {
	zc.state = .accepted
}

// reject marks 0-RTT as rejected by server
pub fn (mut zc ZeroRTTConnection) reject() {
	zc.state = .rejected
	zc.early_data.clear()
	zc.bytes_sent = 0
}

// get_early_data returns all early data to be sent
pub fn (zc &ZeroRTTConnection) get_early_data() []EarlyData {
	return zc.early_data.clone()
}

// AntiReplayCache prevents replay attacks on 0-RTT data
pub struct AntiReplayCache {
mut:
	seen_tokens map[string]time.Time
	window      time.Duration = 10 * time.second
}

pub fn new_anti_replay_cache() AntiReplayCache {
	return AntiReplayCache{
		seen_tokens: map[string]time.Time{}
	}
}

// check_and_store checks if a token has been seen and stores it
pub fn (mut arc AntiReplayCache) check_and_store(token string) bool {
	now := time.now()

	// Check if token was seen recently
	if seen_time := arc.seen_tokens[token] {
		age := now - seen_time
		if age < arc.window {
			return false // Replay detected
		}
	}

	// Store token
	arc.seen_tokens[token] = now

	// Cleanup old tokens
	arc.cleanup()

	return true
}

// cleanup removes old tokens from the cache
fn (mut arc AntiReplayCache) cleanup() {
	now := time.now()
	mut to_remove := []string{}

	for token, seen_time in arc.seen_tokens {
		age := now - seen_time
		if age > arc.window * 2 {
			to_remove << token
		}
	}

	for token in to_remove {
		arc.seen_tokens.delete(token)
	}
}

// ZeroRTTStats tracks statistics for 0-RTT connections
pub struct ZeroRTTStats {
pub mut:
	attempts       u64
	accepted       u64
	rejected       u64
	bytes_sent     u64
	replay_blocked u64
}

pub fn (mut stats ZeroRTTStats) record_attempt() {
	stats.attempts++
}

pub fn (mut stats ZeroRTTStats) record_accepted(bytes u32) {
	stats.accepted++
	stats.bytes_sent += bytes
}

pub fn (mut stats ZeroRTTStats) record_rejected() {
	stats.rejected++
}

pub fn (mut stats ZeroRTTStats) record_replay_blocked() {
	stats.replay_blocked++
}

pub fn (stats &ZeroRTTStats) acceptance_rate() f64 {
	if stats.attempts == 0 {
		return 0.0
	}
	return f64(stats.accepted) / f64(stats.attempts)
}
