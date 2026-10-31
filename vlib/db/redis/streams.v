module redis

// StreamEntry is one stream record. Fields preserve arbitrary binary data in V strings.
// Deleted records read from a group's pending list have deleted set to true.
pub struct StreamEntry {
pub:
	id      string
	fields  map[string]string
	deleted bool
}

// StreamOffset identifies a stream and the last seen ID, or a special ID such as '$' or '>'.
pub struct StreamOffset {
pub:
	key string
	id  string
}

// StreamRead contains the entries returned for one stream by xread or xreadgroup.
pub struct StreamRead {
pub:
	key     string
	entries []StreamEntry
}

// StreamTrim configures trimming by maximum length or minimum ID.
// Set exactly one of maxlen and minid; limit is valid only with approximate trimming.
@[params]
pub struct StreamTrim {
pub:
	maxlen      ?i64
	minid       ?string
	approximate bool
	limit       i64
}

// XAddOptions configures optional stream creation and trimming during xadd.
@[params]
pub struct XAddOptions {
pub:
	nomkstream bool
	trim       StreamTrim
}

// XReadOptions configures a read count and blocking timeout in milliseconds.
// An omitted block does not block; block: 0 waits indefinitely.
@[params]
pub struct XReadOptions {
pub:
	count int
	block ?i64
	noack bool // only valid for xreadgroup
}

// XGroupOptions configures creation or resetting of a group's last delivered ID.
@[params]
pub struct XGroupOptions {
pub:
	mkstream     bool // only valid for xgroup_create
	entries_read ?i64 // Redis 7.0 or newer
}

// XRangeOptions limits the number of records returned by a stream range command.
@[params]
pub struct XRangeOptions {
pub:
	count int
}

// XClaimOptions configures ownership transfer for pending stream entries.
@[params]
pub struct XClaimOptions {
pub:
	idle       ?i64
	time       ?i64
	retrycount ?i64
	force      bool
	lastid     ?string
}

// XAutoClaimOptions configures automatic pending-entry ownership transfer.
@[params]
pub struct XAutoClaimOptions {
pub:
	count  int
	justid bool
}

// StreamAutoClaim contains the next cursor, claimed entries or IDs, and deleted pending IDs.
// Redis versions before 7.0 omit deleted_ids. With justid, only ids is populated.
pub struct StreamAutoClaim {
pub:
	cursor      string
	entries     []StreamEntry
	ids         []string
	deleted_ids []string
}

// StreamPendingConsumer reports how many pending entries belong to a consumer.
pub struct StreamPendingConsumer {
pub:
	consumer string
	count    i64
}

// StreamPending summarizes a consumer group's pending entry list.
pub struct StreamPending {
pub:
	count     i64
	smallest  ?string
	largest   ?string
	consumers []StreamPendingConsumer
}

// StreamPendingEntry describes the owner, idle duration and delivery count of one pending entry.
pub struct StreamPendingEntry {
pub:
	id         string
	consumer   string
	idle_ms    i64
	deliveries i64
}

// XPendingOptions filters extended pending-entry queries by idle time and consumer.
@[params]
pub struct XPendingOptions {
pub:
	idle     ?i64
	consumer ?string
}

// XInfoStreamOptions requests full stream/group metadata and limits its record count.
@[params]
pub struct XInfoStreamOptions {
pub:
	full  bool
	count int
}

fn stream_trim_args(options StreamTrim, required bool) ![]string {
	mut args := []string{}
	if length := options.maxlen {
		if options.minid != none || length < 0 {
			return CommandError{ message: 'stream trim requires one nonnegative MAXLEN or one MINID' }
		}
		args << 'MAXLEN'
		args << if options.approximate { '~' } else { '=' }
		args << length.str()
	} else if id := options.minid {
		args << 'MINID'
		args << if options.approximate { '~' } else { '=' }
		args << id
	} else if required || options.approximate || options.limit != 0 {
		return CommandError{ message: 'stream trim requires MAXLEN or MINID' }
	}
	if options.limit < 0 || (options.limit > 0 && !options.approximate) {
		return CommandError{ message: 'stream trim LIMIT requires approximate trimming and a nonnegative value' }
	}
	if options.limit > 0 {
		args << ['LIMIT', options.limit.str()]
	}
	return args
}

fn stream_map(resp RedisValue, command string) !map[string]RedisValue {
	match resp {
		map[string]RedisValue { return resp }
		RedisMap { return stream_pairs_map(resp.pairs, command) }
		[]RedisValue { return stream_pairs_map(resp, command) }
		else { return ProtocolError{ message: '`${command}()`: invalid map response' } }
	}
}

fn stream_pairs_map(pairs []RedisValue, command string) !map[string]RedisValue {
	if pairs.len % 2 != 0 {
		return ProtocolError{ message: '`${command}()`: invalid field/value response' }
	}
	mut fields := map[string]RedisValue{}
	for i := 0; i < pairs.len; i += 2 {
		fields[bulk_value[string](pairs[i], command)!] = pairs[i + 1]
	}
	return fields
}

fn stream_entries(resp RedisValue, command string) ![]StreamEntry {
	values := array_value(resp, command)!
	mut entries := []StreamEntry{cap: values.len}
	for value in values {
		parts := array_value(value, command)!
		if parts.len != 2 {
			return ProtocolError{ message: '`${command}()`: invalid stream entry' }
		}
		id := bulk_value[string](parts[0], command)!
		if parts[1] is RedisNull {
			entries << StreamEntry{ id: id, deleted: true }
			continue
		}
		values_map := stream_map(parts[1], command)!
		mut fields := map[string]string{}
		for key, field in values_map {
			fields[key] = bulk_value[string](field, command)!
		}
		entries << StreamEntry{ id: id, fields: fields }
	}
	return entries
}

fn stream_reads(resp RedisValue, command string) ![]StreamRead {
	if resp is RedisNull {
		return []StreamRead{}
	}
	mut result := []StreamRead{}
	if resp is []RedisValue {
		for value in resp {
			parts := array_value(value, command)!
			if parts.len != 2 {
				return ProtocolError{ message: '`${command}()`: invalid stream read response' }
			}
			result << StreamRead{
				key:     bulk_value[string](parts[0], command)!
				entries: stream_entries(parts[1], command)!
			}
		}
	} else {
		streams := stream_map(resp, command)!
		for key, entries in streams {
			result << StreamRead{ key: key, entries: stream_entries(entries, command)! }
		}
	}
	return result
}

fn stream_read_args(args []string, streams []StreamOffset, options XReadOptions, group bool) ![]string {
	if streams.len == 0 || options.count < 0 || (options.noack && !group) {
		return CommandError{ message: 'invalid stream read options' }
	}
	mut result := args.clone()
	if options.count > 0 {
		result << ['COUNT', options.count.str()]
	}
	if milliseconds := options.block {
		if milliseconds < 0 {
			return CommandError{ message: 'stream BLOCK timeout must be nonnegative' }
		}
		result << ['BLOCK', milliseconds.str()]
	}
	if options.noack {
		result << 'NOACK'
	}
	result << 'STREAMS'
	for stream in streams { result << stream.key }
	for stream in streams { result << stream.id }
	return result
}

// xadd appends fields under the given ID ('*' selects a server-generated ID).
// NOMKSTREAM returns NilError when the stream does not exist.
pub fn (mut db DB) xadd[T](key string, id string, fields map[string]T, options XAddOptions) !string {
	if fields.len == 0 {
		return CommandError{ message: '`xadd()`: at least one field is required' }
	}
	mut args := ['XADD', key]
	if options.nomkstream { args << 'NOMKSTREAM' }
	args << stream_trim_args(options.trim, false)!
	args << id
	for field, value in fields {
		args << [field, value_string(value, 'xadd')!]
	}
	return db.execute_string(args)
}

// xread retrieves entries after each supplied stream ID, optionally blocking for new entries.
// A timeout or no matching entries returns an empty array.
pub fn (mut db DB) xread(streams []StreamOffset, options XReadOptions) ![]StreamRead {
	resp := db.cmd(...stream_read_args(['XREAD'], streams, options, false)!)!
	if db.pipeline_mode || db.transaction_mode { return []StreamRead{} }
	return stream_reads(resp, 'xread')
}

// xreadgroup retrieves entries for a group consumer. Use '>' for entries never delivered before.
pub fn (mut db DB) xreadgroup(group string, consumer string, streams []StreamOffset, options XReadOptions) ![]StreamRead {
	resp := db.cmd(...stream_read_args(['XREADGROUP', 'GROUP', group, consumer], streams, options, true)!)!
	if db.pipeline_mode || db.transaction_mode { return []StreamRead{} }
	return stream_reads(resp, 'xreadgroup')
}

// xgroup_create creates a consumer group starting at the given ID ('0' includes existing entries).
pub fn (mut db DB) xgroup_create(key string, group string, id string, options XGroupOptions) !string {
	mut args := ['XGROUP', 'CREATE', key, group, id]
	if options.mkstream { args << 'MKSTREAM' }
	if count := options.entries_read { args << ['ENTRIESREAD', count.str()] }
	return db.execute_string(args)
}

// xgroup_createconsumer creates a group consumer and reports whether it was newly created.
pub fn (mut db DB) xgroup_createconsumer(key string, group string, consumer string) !bool {
	return db.execute_i64(['XGROUP', 'CREATECONSUMER', key, group, consumer])! != 0
}

// xgroup_setid resets a group's last delivered ID.
pub fn (mut db DB) xgroup_setid(key string, group string, id string, options XGroupOptions) !string {
	if options.mkstream {
		return CommandError{ message: '`xgroup_setid()`: MKSTREAM is only valid for CREATE' }
	}
	mut args := ['XGROUP', 'SETID', key, group, id]
	if count := options.entries_read { args << ['ENTRIESREAD', count.str()] }
	return db.execute_string(args)
}

// xgroup_destroy removes a group and reports whether it existed.
pub fn (mut db DB) xgroup_destroy(key string, group string) !bool {
	return db.execute_i64(['XGROUP', 'DESTROY', key, group])! != 0
}

// xgroup_delconsumer removes a consumer and returns its number of pending entries.
pub fn (mut db DB) xgroup_delconsumer(key string, group string, consumer string) !i64 {
	return db.execute_i64(['XGROUP', 'DELCONSUMER', key, group, consumer])
}

fn (mut db DB) execute_stream_range(args []string, options XRangeOptions) ![]StreamEntry {
	if options.count < 0 {
		return CommandError{ message: 'stream range COUNT must be nonnegative' }
	}
	mut command := args.clone()
	if options.count > 0 { command << ['COUNT', options.count.str()] }
	resp := db.cmd(...command)!
	if db.pipeline_mode || db.transaction_mode { return []StreamEntry{} }
	return stream_entries(resp, args[0].to_lower())
}

// xrange returns entries in ID order between inclusive start and end ('-' and '+' are unbounded).
pub fn (mut db DB) xrange(key string, start string, end string, options XRangeOptions) ![]StreamEntry {
	return db.execute_stream_range(['XRANGE', key, start, end], options)
}

// xrevrange returns entries in reverse ID order between inclusive end and start.
pub fn (mut db DB) xrevrange(key string, end string, start string, options XRangeOptions) ![]StreamEntry {
	return db.execute_stream_range(['XREVRANGE', key, end, start], options)
}

// xlen returns the number of entries in a stream.
pub fn (mut db DB) xlen(key string) !i64 { return db.execute_i64(['XLEN', key]) }

// xdel deletes entries by ID and returns the number removed.
pub fn (mut db DB) xdel(key string, ids ...string) !i64 {
	mut args := ['XDEL', key]
	args << ids
	return db.execute_i64(args)
}

// xtrim trims a stream by maximum length or minimum ID and returns the number removed.
pub fn (mut db DB) xtrim(key string, options StreamTrim) !i64 {
	mut args := ['XTRIM', key]
	args << stream_trim_args(options, true)!
	return db.execute_i64(args)
}

// xack removes delivered IDs from a group's pending list and returns the number acknowledged.
pub fn (mut db DB) xack(key string, group string, ids ...string) !i64 {
	mut args := ['XACK', key, group]
	args << ids
	return db.execute_i64(args)
}

fn stream_claim_args(key string, group string, consumer string, min_idle_time i64, ids []string, options XClaimOptions) ![]string {
	if min_idle_time < 0 || ids.len == 0 || (options.idle != none && options.time != none) {
		return CommandError{ message: 'invalid stream claim options' }
	}
	mut args := ['XCLAIM', key, group, consumer, min_idle_time.str()]
	args << ids
	if value := options.idle {
		if value < 0 { return CommandError{ message: 'stream claim IDLE must be nonnegative' } }
		args << ['IDLE', value.str()]
	}
	if value := options.time {
		if value < 0 { return CommandError{ message: 'stream claim TIME must be nonnegative' } }
		args << ['TIME', value.str()]
	}
	if value := options.retrycount {
		if value < 0 {
			return CommandError{ message: 'stream claim RETRYCOUNT must be nonnegative' }
		}
		args << ['RETRYCOUNT', value.str()]
	}
	if options.force { args << 'FORCE' }
	if value := options.lastid { args << ['LASTID', value] }
	return args
}

// xclaim transfers ownership of pending entries idle for at least min_idle_time milliseconds.
pub fn (mut db DB) xclaim(key string, group string, consumer string, min_idle_time i64, ids []string, options XClaimOptions) ![]StreamEntry {
	resp := db.cmd(...stream_claim_args(key, group, consumer, min_idle_time, ids, options)!)!
	if db.pipeline_mode || db.transaction_mode { return []StreamEntry{} }
	return stream_entries(resp, 'xclaim')
}

// xclaim_ids transfers ownership like xclaim, returning only IDs without increasing delivery counts.
pub fn (mut db DB) xclaim_ids(key string, group string, consumer string, min_idle_time i64, ids []string, options XClaimOptions) ![]string {
	mut args := stream_claim_args(key, group, consumer, min_idle_time, ids, options)!
	args << 'JUSTID'
	return db.execute_strings(args)
}

// xautoclaim scans and transfers pending entries, returning the next cursor and claimed records.
pub fn (mut db DB) xautoclaim(key string, group string, consumer string, min_idle_time i64, start string, options XAutoClaimOptions) !StreamAutoClaim {
	if min_idle_time < 0 || options.count < 0 {
		return CommandError{ message: 'invalid stream auto claim options' }
	}
	mut args := ['XAUTOCLAIM', key, group, consumer, min_idle_time.str(), start]
	if options.count > 0 { args << ['COUNT', options.count.str()] }
	if options.justid { args << 'JUSTID' }
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode { return StreamAutoClaim{} }
	values := array_value(resp, 'xautoclaim')!
	if values.len != 2 && values.len != 3 {
		return ProtocolError{ message: '`xautoclaim()`: invalid response' }
	}
	mut result := StreamAutoClaim{ cursor: bulk_value[string](values[0], 'xautoclaim')! }
	if options.justid {
		result = StreamAutoClaim{ ...result, ids: string_values(values[1], 'xautoclaim')! }
	} else {
		result = StreamAutoClaim{ ...result, entries: stream_entries(values[1], 'xautoclaim')! }
	}
	if values.len == 3 {
		result = StreamAutoClaim{ ...result, deleted_ids: string_values(values[2], 'xautoclaim')! }
	}
	return result
}

fn stream_integer(resp RedisValue, command string) !i64 {
	match resp {
		i64 { return resp }
		else { return bulk_value[i64](resp, command) }
	}
}

// xpending summarizes entries delivered to a consumer group but not yet acknowledged.
pub fn (mut db DB) xpending(key string, group string) !StreamPending {
	resp := db.cmd('XPENDING', key, group)!
	if db.pipeline_mode || db.transaction_mode { return StreamPending{} }
	values := array_value(resp, 'xpending')!
	if values.len != 4 { return ProtocolError{ message: '`xpending()`: invalid response' } }
	mut smallest := ?string(none)
	mut largest := ?string(none)
	if values[1] !is RedisNull { smallest = bulk_value[string](values[1], 'xpending')! }
	if values[2] !is RedisNull { largest = bulk_value[string](values[2], 'xpending')! }
	mut consumers := []StreamPendingConsumer{}
	if values[3] !is RedisNull {
		for value in array_value(values[3], 'xpending')! {
			parts := array_value(value, 'xpending')!
			if parts.len != 2 {
				return ProtocolError{ message: '`xpending()`: invalid consumer response' }
			}
			consumers << StreamPendingConsumer{
				consumer: bulk_value[string](parts[0], 'xpending')!
				count:    stream_integer(parts[1], 'xpending')!
			}
		}
	}
	return StreamPending{
		count:     stream_integer(values[0], 'xpending')!
		smallest:  smallest
		largest:   largest
		consumers: consumers
	}
}

// xpending_range returns pending-entry details in ID order, with optional idle and consumer filters.
pub fn (mut db DB) xpending_range(key string, group string, start string, end string, count int, options XPendingOptions) ![]StreamPendingEntry {
	if count <= 0 { return CommandError{ message: '`xpending_range()`: COUNT must be positive' } }
	mut args := ['XPENDING', key, group]
	if idle := options.idle {
		if idle < 0 {
			return CommandError{ message: '`xpending_range()`: IDLE must be nonnegative' }
		}
		args << ['IDLE', idle.str()]
	}
	args << [start, end, count.str()]
	if consumer := options.consumer { args << consumer }
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode { return []StreamPendingEntry{} }
	mut result := []StreamPendingEntry{}
	for value in array_value(resp, 'xpending_range')! {
		parts := array_value(value, 'xpending_range')!
		if parts.len != 4 {
			return ProtocolError{ message: '`xpending_range()`: invalid pending entry' }
		}
		result << StreamPendingEntry{
			id:         bulk_value[string](parts[0], 'xpending_range')!
			consumer:   bulk_value[string](parts[1], 'xpending_range')!
			idle_ms:    stream_integer(parts[2], 'xpending_range')!
			deliveries: stream_integer(parts[3], 'xpending_range')!
		}
	}
	return result
}

// xinfo_stream returns stream metadata as a map, including Redis-version-specific fields.
pub fn (mut db DB) xinfo_stream(key string, options XInfoStreamOptions) !map[string]RedisValue {
	if options.count < 0 || (options.count > 0 && !options.full) {
		return CommandError{ message: '`xinfo_stream()`: COUNT requires FULL and must be nonnegative' }
	}
	mut args := ['XINFO', 'STREAM', key]
	if options.full { args << 'FULL' }
	if options.count > 0 { args << ['COUNT', options.count.str()] }
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode { return map[string]RedisValue{} }
	return stream_map(resp, 'xinfo_stream')
}

fn (mut db DB) execute_stream_info(args []string) ![]map[string]RedisValue {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode { return []map[string]RedisValue{} }
	mut result := []map[string]RedisValue{}
	for value in array_value(resp, 'xinfo')! { result << stream_map(value, 'xinfo')! }
	return result
}

// xinfo_groups returns metadata for each stream consumer group.
pub fn (mut db DB) xinfo_groups(key string) ![]map[string]RedisValue {
	return db.execute_stream_info(['XINFO', 'GROUPS', key])
}

// xinfo_consumers returns metadata for each consumer in a stream group.
pub fn (mut db DB) xinfo_consumers(key string, group string) ![]map[string]RedisValue {
	return db.execute_stream_info(['XINFO', 'CONSUMERS', key, group])
}
