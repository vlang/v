module redis

// GetExMode selects the expiration operation performed by getex.
pub enum GetExMode {
	none
	ex
	px
	exat
	pxat
	persist
}

// GetExOptions configures getex expiration in seconds, milliseconds, or Unix timestamps.
@[params]
pub struct GetExOptions {
pub:
	mode  GetExMode
	value i64
}

// ScanOptions configures optional pattern matching and the suggested scan batch size.
@[params]
pub struct ScanOptions {
pub:
	match ?string
	count int
}

// CopyOptions selects an optional destination database and whether copy replaces the destination.
@[params]
pub struct CopyOptions {
pub:
	database ?int
	replace  bool
}

// RestoreOptions configures replacement, absolute expiration, and eviction metadata for restore.
// `idletime` and `freq` are mutually exclusive.
@[params]
pub struct RestoreOptions {
pub:
	replace  bool
	absttl   bool
	idletime ?i64
	freq     ?int
}

// SortOptions configures external weights, pagination, projected values, and ordering for sort.
@[params]
pub struct SortOptions {
pub:
	by     ?string
	offset i64
	count  ?i64
	get    []string
	desc   bool
	alpha  bool
}

// HashField contains a hash field and its value.
pub struct HashField {
pub:
	field string
	value string
}

// LcsOptions configures the minimum length of matches reported by lcs_idx.
@[params]
pub struct LcsOptions {
pub:
	min_match_len i64
}

// LcsMatch contains the inclusive byte ranges of one matching block in both keys.
pub struct LcsMatch {
pub:
	first_start  i64
	first_end    i64
	second_start i64
	second_end   i64
	length       i64
}

// LcsResult contains matching blocks in the order Redis reports them, and the total LCS length.
pub struct LcsResult {
pub:
	matches []LcsMatch
	length  i64
}

// DelexCondition selects the comparison that delex requires before deleting a key.
pub enum DelexCondition {
	none
	ifeq
	ifne
	ifdeq
	ifdne
}

// DelexOptions compares a key's value, or its digest from `digest`, before deletion.
@[params]
pub struct DelexOptions {
pub:
	condition DelexCondition
	value     string
}

// IncrExOptions configures integer increx bounds, saturation, and expiration.
// `expiration` uses the getex modes; timed modes require a positive `expiration_value`.
@[params]
pub struct IncrExOptions {
pub:
	lbound           ?i64
	ubound           ?i64
	saturate         bool
	expiration       GetExMode
	expiration_value i64
	enx              bool
}

// IncrExFloatOptions configures floating point increx bounds, saturation, and expiration.
// `expiration` uses the getex modes; timed modes require a positive `expiration_value`.
@[params]
pub struct IncrExFloatOptions {
pub:
	lbound           ?f64
	ubound           ?f64
	saturate         bool
	expiration       GetExMode
	expiration_value i64
	enx              bool
}

fn value_string[T](value T, command string) !string {
	$if T is string {
		return value
	} $else $if T is $int {
		return value.str()
	} $else $if T is []u8 {
		return value.bytestr()
	} $else {
		return CommandError{ message: '`${command}()`: unsupported value type. Allowed: number, string, []u8' }
	}
}

fn validate_bulk_type[T](command string) ! {
	$if T !is string && T !is $int && T !is []u8 {
		return CommandError{ message: '`${command}()`: unsupported return type. Allowed: number, string, []u8' }
	}
}

// Keep unsigned parsing outside generic instantiation so its intermediate types stay u64.
fn unsigned_value(data string) u64 {
	return data.u64()
}

fn bulk_value[T](resp RedisValue, command string) !T {
	validate_bulk_type[T](command)!
	$if T is []u8 {
		if resp is []u8 {
			return resp
		}
	}
	data := match resp {
		[]u8 { resp.bytestr() }
		string { resp }
		RedisNull { return NilError{ message: '`${command}()`: value not found' } }
		else { return ProtocolError{ message: '`${command}()`: unexpected response type' } }
	}
	$if T is string {
		return data
	} $else $if T is u64 || T is usize {
		return T(unsigned_value(data))
	} $else $if T is $int {
		return T(data.i64())
	} $else $if T is []u8 {
		return data.bytes()
	} $else {
		return CommandError{ message: '`${command}()`: unsupported return type' }
	}
}

fn array_value(resp RedisValue, command string) ![]RedisValue {
	match resp {
		[]RedisValue { return resp }
		else { return ProtocolError{ message: '`${command}()`: unexpected response type' } }
	}
}

fn string_values(resp RedisValue, command string) ![]string {
	values := array_value(resp, command)!
	mut result := []string{cap: values.len}
	for value in values {
		result << bulk_value[string](value, command)!
	}
	return result
}

fn nullable_values[T](resp RedisValue, command string) ![]?T {
	values := array_value(resp, command)!
	mut result := []?T{cap: values.len}
	for value in values {
		if value is RedisNull {
			result << none
		} else {
			result << ?T(bulk_value[T](value, command)!)
		}
	}
	return result
}

fn (mut db DB) execute_i64(args []string) !i64 {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return 0
	}
	match resp {
		i64 { return resp }
		else {
			return ProtocolError{ message: '`${args[0].to_lower()}()`: unexpected response type' }
		}
	}
}

fn (mut db DB) execute_string(args []string) !string {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return ''
	}
	return bulk_value[string](resp, args[0].to_lower())
}

fn (mut db DB) execute_strings(args []string) ![]string {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []string{}
	}
	return string_values(resp, args[0].to_lower())
}

fn (mut db DB) execute_bulk[T](args []string) !T {
	validate_bulk_type[T](args[0].to_lower())!
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return T{}
	}
	return bulk_value[T](resp, args[0].to_lower())
}

fn (mut db DB) execute_nullable[T](args []string) ![]?T {
	validate_bulk_type[T](args[0].to_lower())!
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []?T{}
	}
	return nullable_values[T](resp, args[0].to_lower())
}

fn (mut db DB) execute_f64(args []string) !f64 {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return 0.0
	}
	match resp {
		f64 { return resp }
		else { return bulk_value[string](resp, args[0].to_lower())!.f64() }
	}
}

fn multi_set_args[T](command string, values map[string]T) ![]string {
	$if T !is string && T !is $int && T !is []u8 {
		return CommandError{ message: '`${command.to_lower()}()`: unsupported value type. Allowed: number, string, []u8' }
	}
	mut args := [command]
	for key, value in values {
		args << key
		args << value_string(value, command.to_lower())!
	}
	return args
}

fn scan_args(args []string, options ScanOptions) ![]string {
	if options.count < 0 {
		return CommandError{ message: '`${args[0].to_lower()}()`: count must not be negative' }
	}
	mut result := args.clone()
	if pattern := options.match {
		result << 'MATCH'
		result << pattern
	}
	if options.count > 0 {
		result << 'COUNT'
		result << options.count.str()
	}
	return result
}

fn (mut db DB) execute_scan(args []string) !(string, []string) {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return '', []string{}
	}
	command := args[0].to_lower()
	values := array_value(resp, command)!
	if values.len != 2 {
		return ProtocolError{ message: '`${command}()`: invalid scan response' }
	}
	return bulk_value[string](values[0], command)!, string_values(values[1], command)!
}

// mget retrieves values in key order, preserving missing keys as none.
// Supported return types are string, integer types, and []u8.
pub fn (mut db DB) mget[T](keys ...string) ![]?T {
	mut args := ['MGET']
	args << keys
	return db.execute_nullable[T](args)
}

// mset atomically stores multiple keys. Supported value types are string, integer types, and []u8.
pub fn (mut db DB) mset[T](values map[string]T) !string {
	return db.execute_string(multi_set_args('MSET', values)!)
}

// msetnx atomically stores multiple keys only when none of them exists.
pub fn (mut db DB) msetnx[T](values map[string]T) !bool {
	return db.execute_i64(multi_set_args('MSETNX', values)!)! != 0
}

// setnx stores a key only when it does not exist.
pub fn (mut db DB) setnx[T](key string, value T) !bool {
	return db.execute_i64(['SETNX', key, value_string(value, 'setnx')!])! != 0
}

// setex stores a value with an expiration time in seconds.
pub fn (mut db DB) setex[T](key string, seconds i64, value T) !string {
	return db.execute_string(['SETEX', key, seconds.str(), value_string(value, 'setex')!])
}

// psetex stores a value with an expiration time in milliseconds.
pub fn (mut db DB) psetex[T](key string, milliseconds i64, value T) !string {
	return db.execute_string(['PSETEX', key, milliseconds.str(), value_string(value, 'psetex')!])
}

// getdel retrieves and deletes a string key, returning an error if the key is missing.
pub fn (mut db DB) getdel[T](key string) !T {
	return db.execute_bulk[T](['GETDEL', key])
}

fn expiration_args(command string, mode GetExMode, value i64) ![]string {
	match mode {
		.none, .persist {
			if value != 0 {
				return CommandError{ message: '`${command}()`: expiration value requires a timed mode' }
			}
			return if mode == .persist { ['PERSIST'] } else { []string{} }
		}
		.ex, .px, .exat, .pxat {
			if value <= 0 {
				return CommandError{ message: '`${command}()`: expiration value must be positive' }
			}
			return [mode.str().to_upper(), value.str()]
		}
	}
}

// getex retrieves a string key and optionally changes or removes its expiration.
// Missing keys return an error. Timed modes require a positive value.
pub fn (mut db DB) getex[T](key string, options GetExOptions) !T {
	mut args := ['GETEX', key]
	args << expiration_args('getex', options.mode, options.value)!
	return db.execute_bulk[T](args)
}

// getset stores a value and returns the previous one. GETSET is deprecated since Redis 6.2.
// The value is stored even when the key was missing, in which case NilError is returned.
pub fn (mut db DB) getset[T](key string, value T) !T {
	return db.execute_bulk[T](['GETSET', key, value_string(value, 'getset')!])
}

// incrby increments an integer key by the given amount.
pub fn (mut db DB) incrby(key string, increment i64) !i64 {
	return db.execute_i64(['INCRBY', key, increment.str()])
}

// decrby decrements an integer key by the given amount.
pub fn (mut db DB) decrby(key string, decrement i64) !i64 {
	return db.execute_i64(['DECRBY', key, decrement.str()])
}

// incrbyfloat increments a numeric key by the given floating point amount.
pub fn (mut db DB) incrbyfloat(key string, increment f64) !f64 {
	return db.execute_f64(['INCRBYFLOAT', key, increment.str()])
}

// exists counts the specified keys that exist, counting repeated keys separately.
pub fn (mut db DB) exists(keys ...string) !i64 {
	mut args := ['EXISTS']
	args << keys
	return db.execute_i64(args)
}

// ttl returns the remaining lifetime in seconds, or -1 without expiration and -2 for a missing key.
pub fn (mut db DB) ttl(key string) !i64 {
	return db.execute_i64(['TTL', key])
}

// pttl returns the remaining lifetime in milliseconds, or -1 without expiration and -2 if missing.
pub fn (mut db DB) pttl(key string) !i64 {
	return db.execute_i64(['PTTL', key])
}

// pexpire sets a key's expiration in milliseconds and reports whether it was set.
pub fn (mut db DB) pexpire(key string, milliseconds i64) !bool {
	return db.execute_i64(['PEXPIRE', key, milliseconds.str()])! != 0
}

// expireat sets a key's expiration to a Unix timestamp in seconds.
pub fn (mut db DB) expireat(key string, timestamp i64) !bool {
	return db.execute_i64(['EXPIREAT', key, timestamp.str()])! != 0
}

// pexpireat sets a key's expiration to a Unix timestamp in milliseconds.
pub fn (mut db DB) pexpireat(key string, timestamp i64) !bool {
	return db.execute_i64(['PEXPIREAT', key, timestamp.str()])! != 0
}

// persist removes a key's expiration and reports whether an expiration was removed.
pub fn (mut db DB) persist(key string) !bool {
	return db.execute_i64(['PERSIST', key])! != 0
}

// unlink removes keys, reclaiming their memory asynchronously.
pub fn (mut db DB) unlink(keys ...string) !i64 {
	mut args := ['UNLINK']
	args << keys
	return db.execute_i64(args)
}

// keys returns all keys matching a pattern. Use scan for incremental iteration over large databases.
pub fn (mut db DB) keys(pattern string) ![]string {
	return db.execute_strings(['KEYS', pattern])
}

// scan returns the next cursor and matching keys. Continue until the cursor is '0'.
pub fn (mut db DB) scan(cursor string, options ScanOptions) !(string, []string) {
	return db.execute_scan(scan_args(['SCAN', cursor], options)!)
}

// key_type returns the Redis type of a key, or 'none' if the key does not exist.
pub fn (mut db DB) key_type(key string) !string {
	return db.execute_string(['TYPE', key])
}

// rename renames a key, replacing any existing destination key.
pub fn (mut db DB) rename(key string, new_key string) !string {
	return db.execute_string(['RENAME', key, new_key])
}

// renamenx renames a key only when the destination does not exist.
pub fn (mut db DB) renamenx(key string, new_key string) !bool {
	return db.execute_i64(['RENAMENX', key, new_key])! != 0
}

// copy copies a key, optionally into another database, and reports whether it was copied.
// Existing destinations are kept unless `replace` is set. Requires Redis 6.2 or later.
pub fn (mut db DB) copy(source string, destination string, options CopyOptions) !bool {
	mut args := ['COPY', source, destination]
	if database := options.database {
		if database < 0 {
			return CommandError{ message: '`copy()`: database must not be negative' }
		}
		args << ['DB', database.str()]
	}
	if options.replace {
		args << 'REPLACE'
	}
	return db.execute_i64(args)! != 0
}

// move moves a key to another database and reports whether it was moved.
pub fn (mut db DB) move(key string, database int) !bool {
	if database < 0 {
		return CommandError{ message: '`move()`: database must not be negative' }
	}
	return db.execute_i64(['MOVE', key, database.str()])! != 0
}

// touch updates the last access time of keys and returns the number of existing keys.
pub fn (mut db DB) touch(keys ...string) !i64 {
	mut args := ['TOUCH']
	args << keys
	return db.execute_i64(args)
}

// dump serializes a key's value in Redis's opaque format for restore.
// Missing keys return NilError.
pub fn (mut db DB) dump(key string) ![]u8 {
	return db.execute_bulk[[]u8](['DUMP', key])
}

// restore creates a key from dump output. A zero ttl creates the key without expiration;
// otherwise ttl is in milliseconds, or a Unix timestamp in milliseconds with `absttl`.
pub fn (mut db DB) restore(key string, ttl i64, serialized []u8, options RestoreOptions) !string {
	if ttl < 0 {
		return CommandError{ message: '`restore()`: ttl must not be negative' }
	}
	mut args := ['RESTORE', key, ttl.str(), serialized.bytestr()]
	if options.replace {
		args << 'REPLACE'
	}
	if options.absttl {
		args << 'ABSTTL'
	}
	if seconds := options.idletime {
		if options.freq != none {
			return CommandError{ message: '`restore()`: idletime and freq are mutually exclusive' }
		}
		if seconds < 0 {
			return CommandError{ message: '`restore()`: idletime must not be negative' }
		}
		args << ['IDLETIME', seconds.str()]
	}
	if frequency := options.freq {
		if frequency < 0 || frequency > 255 {
			return CommandError{ message: '`restore()`: freq must be between 0 and 255' }
		}
		args << ['FREQ', frequency.str()]
	}
	return db.execute_string(args)
}

fn sort_args(command string, key string, destination ?string, options SortOptions) ![]string {
	mut args := [command, key]
	if pattern := options.by {
		args << ['BY', pattern]
	}
	args = zrange_limit_args(args, ZRangeLimit{ offset: options.offset, count: options.count })!
	for pattern in options.get {
		args << ['GET', pattern]
	}
	if options.desc {
		args << 'DESC'
	}
	if options.alpha {
		args << 'ALPHA'
	}
	if key_name := destination {
		args << ['STORE', key_name]
	}
	return args
}

// sort returns the sorted elements of a list, set, or sorted set. Entries are none when a
// `get` pattern references a missing key or hash field. Use `alpha` for non-numeric elements.
// Supported return types are string, integer types, and []u8.
pub fn (mut db DB) sort[T](key string, options SortOptions) ![]?T {
	return db.execute_nullable[T](sort_args('SORT', key, none, options)!)
}

// sort_ro is the read-only variant of sort, usable on replicas. Requires Redis 7.0 or later.
pub fn (mut db DB) sort_ro[T](key string, options SortOptions) ![]?T {
	return db.execute_nullable[T](sort_args('SORT_RO', key, none, options)!)
}

// sort_store sorts a key into destination as a list and returns the number of stored elements.
pub fn (mut db DB) sort_store(key string, destination string, options SortOptions) !i64 {
	return db.execute_i64(sort_args('SORT', key, destination, options)!)
}

// hdel removes hash fields and returns the number removed.
pub fn (mut db DB) hdel(key string, fields ...string) !i64 {
	mut args := ['HDEL', key]
	args << fields
	return db.execute_i64(args)
}

// hexists reports whether a hash field exists.
pub fn (mut db DB) hexists(key string, field string) !bool {
	return db.execute_i64(['HEXISTS', key, field])! != 0
}

// hkeys returns all field names in a hash.
pub fn (mut db DB) hkeys(key string) ![]string {
	return db.execute_strings(['HKEYS', key])
}

// hvals returns all values in a hash as strings.
pub fn (mut db DB) hvals(key string) ![]string {
	return db.execute_strings(['HVALS', key])
}

// hlen returns the number of fields in a hash.
pub fn (mut db DB) hlen(key string) !i64 {
	return db.execute_i64(['HLEN', key])
}

// hstrlen returns the byte length of a hash field's value, or zero for a missing field.
pub fn (mut db DB) hstrlen(key string, field string) !i64 {
	return db.execute_i64(['HSTRLEN', key, field])
}

// hsetnx stores a hash field only when it does not exist.
pub fn (mut db DB) hsetnx[T](key string, field string, value T) !bool {
	return db.execute_i64(['HSETNX', key, field, value_string(value, 'hsetnx')!])! != 0
}

// hincrby increments an integer hash field by the given amount.
pub fn (mut db DB) hincrby(key string, field string, increment i64) !i64 {
	return db.execute_i64(['HINCRBY', key, field, increment.str()])
}

// hincrbyfloat increments a numeric hash field by the given floating point amount.
pub fn (mut db DB) hincrbyfloat(key string, field string, increment f64) !f64 {
	return db.execute_f64(['HINCRBYFLOAT', key, field, increment.str()])
}

// hmget retrieves hash field values in field order, preserving missing fields as none.
// Supported return types are string, integer types, and []u8.
pub fn (mut db DB) hmget[T](key string, fields ...string) ![]?T {
	mut args := ['HMGET', key]
	args << fields
	return db.execute_nullable[T](args)
}

// hscan returns the next cursor and alternating field/value strings for a hash.
// Continue until the cursor is '0'.
pub fn (mut db DB) hscan(key string, cursor string, options ScanOptions) !(string, []string) {
	return db.execute_scan(scan_args(['HSCAN', key, cursor], options)!)
}

// hrandfield returns a random field name, or NilError if the hash is missing.
pub fn (mut db DB) hrandfield(key string) !string {
	return db.data_structure_string(['HRANDFIELD', key])
}

// hrandfield_count returns random field names; negative counts permit repeated fields.
pub fn (mut db DB) hrandfield_count(key string, count int) ![]string {
	return db.execute_strings(['HRANDFIELD', key, count.str()])
}

fn hash_field(field RedisValue, value RedisValue) !HashField {
	return HashField{
		field: bulk_value[string](field, 'hrandfield')!
		value: bulk_value[string](value, 'hrandfield')!
	}
}

// hrandfield_withvalues returns random fields with their values; negative counts permit repeats.
pub fn (mut db DB) hrandfield_withvalues(key string, count int) ![]HashField {
	resp := db.cmd('HRANDFIELD', key, count.str(), 'WITHVALUES')!
	if db.pipeline_mode || db.transaction_mode {
		return []HashField{}
	}
	values := array_value(resp, 'hrandfield')!
	mut result := []HashField{cap: values.len}
	if values.len > 0 && values[0] is []RedisValue {
		for value in values {
			pair := array_value(value, 'hrandfield')!
			if pair.len != 2 {
				return ProtocolError{ message: '`hrandfield()`: invalid field/value pair' }
			}
			result << hash_field(pair[0], pair[1])!
		}
		return result
	}
	if values.len % 2 != 0 {
		return ProtocolError{ message: '`hrandfield()`: invalid field/value response' }
	}
	for i := 0; i < values.len; i += 2 {
		result << hash_field(values[i], values[i + 1])!
	}
	return result
}

// append appends a value to a string key and returns the resulting byte length.
pub fn (mut db DB) append[T](key string, value T) !i64 {
	return db.execute_i64(['APPEND', key, value_string(value, 'append')!])
}

// strlen returns the byte length of a string key, or zero if the key is missing.
pub fn (mut db DB) strlen(key string) !i64 {
	return db.execute_i64(['STRLEN', key])
}

// getrange retrieves a string key's byte range using inclusive start and end offsets.
pub fn (mut db DB) getrange[T](key string, start i64, end i64) !T {
	return db.execute_bulk[T](['GETRANGE', key, start.str(), end.str()])
}

// setrange overwrites a string key starting at the byte offset and returns its resulting length.
pub fn (mut db DB) setrange[T](key string, offset i64, value T) !i64 {
	return db.execute_i64(['SETRANGE', key, offset.str(), value_string(value, 'setrange')!])
}

// substr retrieves a byte range like getrange. SUBSTR is deprecated since Redis 2.0.
pub fn (mut db DB) substr[T](key string, start i64, end i64) !T {
	return db.execute_bulk[T](['SUBSTR', key, start.str(), end.str()])
}

// lcs returns the longest common subsequence of two string keys; missing keys are empty.
// Requires Redis 7.0 or later.
pub fn (mut db DB) lcs(key1 string, key2 string) !string {
	return db.execute_string(['LCS', key1, key2])
}

// lcs_len returns the length of the longest common subsequence of two string keys.
pub fn (mut db DB) lcs_len(key1 string, key2 string) !i64 {
	return db.execute_i64(['LCS', key1, key2, 'LEN'])
}

fn lcs_range(value RedisValue) !(i64, i64) {
	bounds := array_value(value, 'lcs')!
	if bounds.len != 2 {
		return ProtocolError{ message: '`lcs()`: invalid match range' }
	}
	return stream_integer(bounds[0], 'lcs')!, stream_integer(bounds[1], 'lcs')!
}

// lcs_idx returns the matching blocks of the longest common subsequence and its total length.
// Blocks shorter than `min_match_len` are omitted from matches but still count toward length.
pub fn (mut db DB) lcs_idx(key1 string, key2 string, options LcsOptions) !LcsResult {
	if options.min_match_len < 0 {
		return CommandError{ message: '`lcs()`: min_match_len must not be negative' }
	}
	mut args := ['LCS', key1, key2, 'IDX']
	if options.min_match_len > 0 {
		args << ['MINMATCHLEN', options.min_match_len.str()]
	}
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return LcsResult{}
	}
	fields := stream_map(resp, 'lcs')!
	reported := fields['matches'] or { return ProtocolError{ message: '`lcs()`: missing matches' } }
	total := fields['len'] or { return ProtocolError{ message: '`lcs()`: missing length' } }
	mut matches := []LcsMatch{}
	for item in array_value(reported, 'lcs')! {
		ranges := array_value(item, 'lcs')!
		if ranges.len < 2 {
			return ProtocolError{ message: '`lcs()`: invalid match' }
		}
		first_start, first_end := lcs_range(ranges[0])!
		second_start, second_end := lcs_range(ranges[1])!
		matches << LcsMatch{
			first_start:  first_start
			first_end:    first_end
			second_start: second_start
			second_end:   second_end
			length:       first_end - first_start + 1
		}
	}
	return LcsResult{ matches: matches, length: stream_integer(total, 'lcs')! }
}

// delex deletes a key and reports whether it was deleted. A condition compares the current
// string value, or its `digest`, and leaves the key unchanged on mismatch. Requires Redis 8.4.
pub fn (mut db DB) delex(key string, options DelexOptions) !bool {
	mut args := ['DELEX', key]
	if options.condition == .none {
		if options.value != '' {
			return CommandError{ message: '`delex()`: value requires a condition' }
		}
	} else {
		args << [options.condition.str().to_upper(), options.value]
	}
	return db.execute_i64(args)! != 0
}

// digest returns the hexadecimal hash digest of a string value for conditional delex.
// Missing keys return NilError. Requires Redis 8.4 or later.
pub fn (mut db DB) digest(key string) !string {
	return db.execute_bulk[string](['DIGEST', key])
}

fn increx_options_args(saturate bool, mode GetExMode, value i64, enx bool) ![]string {
	mut args := []string{}
	if saturate {
		args << 'SATURATE'
	}
	args << expiration_args('increx', mode, value)!
	if enx {
		if mode in [.none, .persist] {
			return CommandError{ message: '`increx()`: enx requires a timed expiration' }
		}
		args << 'ENX'
	}
	return args
}

fn (mut db DB) execute_increx(args []string) !(RedisValue, RedisValue) {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return RedisValue(i64(0)), RedisValue(i64(0))
	}
	values := array_value(resp, 'increx')!
	if values.len != 2 {
		return ProtocolError{ message: '`increx()`: invalid response' }
	}
	return values[0], values[1]
}

// increx atomically increments an integer key, creating it from zero, and returns the new value
// and the applied increment. Out-of-bounds updates are skipped and apply zero unless `saturate`
// caps them at the bound. Expiration options are applied atomically. Requires Redis 8.8 or later.
pub fn (mut db DB) increx(key string, increment i64, options IncrExOptions) !(i64, i64) {
	mut args := ['INCREX', key, 'BYINT', increment.str()]
	if bound := options.lbound {
		args << ['LBOUND', bound.str()]
	}
	if bound := options.ubound {
		args << ['UBOUND', bound.str()]
	}
	args << increx_options_args(options.saturate, options.expiration, options.expiration_value,
		options.enx)!
	value, applied := db.execute_increx(args)!
	return stream_integer(value, 'increx')!, stream_integer(applied, 'increx')!
}

// increx_float is increx with a floating point increment and bounds. Integer values are promoted.
pub fn (mut db DB) increx_float(key string, increment f64, options IncrExFloatOptions) !(f64, f64) {
	mut args := ['INCREX', key, 'BYFLOAT', increment.str()]
	if bound := options.lbound {
		args << ['LBOUND', bound.str()]
	}
	if bound := options.ubound {
		args << ['UBOUND', bound.str()]
	}
	args << increx_options_args(options.saturate, options.expiration, options.expiration_value,
		options.enx)!
	value, applied := db.execute_increx(args)!
	return score_value(value, 'increx')!, score_value(applied, 'increx')!
}
