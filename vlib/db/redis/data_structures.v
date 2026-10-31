module redis

// ListSide selects the end of a list for pop and move operations.
pub enum ListSide {
	left
	right
}

// ListInsertPosition selects where linsert places an element relative to its pivot.
pub enum ListInsertPosition {
	before
	after
}

// ListPop contains the source key and the elements returned by a blocking or multi-key pop.
pub struct ListPop {
pub:
	key    string
	values []string
}

// ZMember contains a sorted set member and its score.
pub struct ZMember {
pub:
	member string
	score  f64
}

// ZPop contains the source key and sorted set member returned by a blocking pop.
pub struct ZPop {
pub:
	key   string
	value ZMember
}

// ZAddOptions configures conditional updates and whether zadd counts changed members.
@[params]
pub struct ZAddOptions {
pub:
	nx      bool
	xx      bool
	gt      bool
	lt      bool
	changed bool
}

// ZRangeOptions configures the direction of a rank-based sorted set range.
@[params]
pub struct ZRangeOptions {
pub:
	rev bool
}

// ZRangeLimit configures an optional LIMIT for score or lexicographic ranges.
@[params]
pub struct ZRangeLimit {
pub:
	offset i64
	count  ?i64
}

// WeightedKey selects a sorted set and the multiplier applied to its scores.
pub struct WeightedKey {
pub:
	key    string
	weight f64 = 1.0
}

// ZAggregate selects how sorted set union and intersection combine scores.
pub enum ZAggregate {
	sum
	min
	max
}

// ZCombineOptions configures score aggregation in sorted set union and intersection.
@[params]
pub struct ZCombineOptions {
pub:
	aggregate ZAggregate
}

fn data_structure_args(command string, key string, values []string) []string {
	mut args := [command, key]
	args << values
	return args
}

fn (mut db DB) data_structure_string(args []string) !string {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return ''
	}
	if resp is RedisNull {
		return NilError{ message: '`${args[0].to_lower()}()`: value not found' }
	}
	return bulk_value[string](resp, args[0].to_lower())
}

fn set_values(resp RedisValue, command string) ![]string {
	if resp is RedisSet {
		return string_values(RedisValue(resp.elements), command)
	}
	return string_values(resp, command)
}

fn (mut db DB) execute_set(args []string) ![]string {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []string{}
	}
	return set_values(resp, args[0].to_lower())
}

fn (mut db DB) execute_pop_count(args []string) ![]string {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode || resp is RedisNull {
		return []string{}
	}
	return set_values(resp, args[0].to_lower())
}

fn (mut db DB) execute_list_pop(args []string, multiple bool) !ListPop {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return ListPop{}
	}
	if resp is RedisNull {
		return NilError{ message: '`${args[0].to_lower()}()`: no element available' }
	}
	command := args[0].to_lower()
	values := array_value(resp, command)!
	if values.len != 2 {
		return ProtocolError{ message: '`${command}()`: invalid list pop response' }
	}
	key := bulk_value[string](values[0], command)!
	if multiple {
		return ListPop{ key: key, values: string_values(values[1], command)! }
	}
	return ListPop{ key: key, values: [bulk_value[string](values[1], command)!] }
}

fn list_multi_pop_args(command string, keys []string, side ListSide, count int) ![]string {
	if keys.len == 0 || count <= 0 {
		return error('`${command.to_lower()}()`: keys and a positive count are required')
	}
	mut args := [command, keys.len.str()]
	args << keys
	args << [side.str().to_upper(), 'COUNT', count.str()]
	return args
}

// lpush prepends values and returns the resulting list length.
pub fn (mut db DB) lpush(key string, values ...string) !i64 {
	return db.execute_i64(data_structure_args('LPUSH', key, values))
}

// rpush appends values and returns the resulting list length.
pub fn (mut db DB) rpush(key string, values ...string) !i64 {
	return db.execute_i64(data_structure_args('RPUSH', key, values))
}

// lpushx prepends values only when the list already exists.
pub fn (mut db DB) lpushx(key string, values ...string) !i64 {
	return db.execute_i64(data_structure_args('LPUSHX', key, values))
}

// rpushx appends values only when the list already exists.
pub fn (mut db DB) rpushx(key string, values ...string) !i64 {
	return db.execute_i64(data_structure_args('RPUSHX', key, values))
}

// lpop removes the first list element, returning NilError for an empty or missing list.
pub fn (mut db DB) lpop(key string) !string {
	return db.data_structure_string(['LPOP', key])
}

// rpop removes the last list element, returning NilError for an empty or missing list.
pub fn (mut db DB) rpop(key string) !string {
	return db.data_structure_string(['RPOP', key])
}

// lpop_count removes up to count elements from the start, returning an empty array if missing.
pub fn (mut db DB) lpop_count(key string, count int) ![]string {
	return db.execute_pop_count(['LPOP', key, count.str()])
}

// rpop_count removes up to count elements from the end, returning an empty array if missing.
pub fn (mut db DB) rpop_count(key string, count int) ![]string {
	return db.execute_pop_count(['RPOP', key, count.str()])
}

// rpoplpush moves the last source element to the start of destination. Prefer lmove.
pub fn (mut db DB) rpoplpush(source string, destination string) !string {
	return db.data_structure_string(['RPOPLPUSH', source, destination])
}

// llen returns the number of elements in a list.
pub fn (mut db DB) llen(key string) !i64 {
	return db.execute_i64(['LLEN', key])
}

// lrange returns elements at inclusive offsets, supporting negative offsets from the end.
pub fn (mut db DB) lrange(key string, start i64, stop i64) ![]string {
	return db.execute_strings(['LRANGE', key, start.str(), stop.str()])
}

// lindex returns the element at an offset, or NilError if the offset or list is missing.
pub fn (mut db DB) lindex(key string, index i64) !string {
	return db.data_structure_string(['LINDEX', key, index.str()])
}

// lset replaces the element at an offset.
pub fn (mut db DB) lset(key string, index i64, value string) !string {
	return db.execute_string(['LSET', key, index.str(), value])
}

// ltrim retains only the elements at inclusive offsets.
pub fn (mut db DB) ltrim(key string, start i64, stop i64) !string {
	return db.execute_string(['LTRIM', key, start.str(), stop.str()])
}

// linsert inserts a value relative to a pivot, returning -1 if the pivot is missing.
pub fn (mut db DB) linsert(key string, position ListInsertPosition, pivot string, value string) !i64 {
	return db.execute_i64(['LINSERT', key, position.str().to_upper(), pivot, value])
}

// lrem removes matching values. Count zero removes all; negative counts start at the end.
pub fn (mut db DB) lrem(key string, count i64, value string) !i64 {
	return db.execute_i64(['LREM', key, count.str(), value])
}

// lpos returns the first matching index, or NilError if the value is absent.
pub fn (mut db DB) lpos(key string, value string) !i64 {
	resp := db.cmd('LPOS', key, value)!
	if db.pipeline_mode || db.transaction_mode {
		return 0
	}
	if resp is RedisNull {
		return NilError{ message: '`lpos()`: value not found' }
	}
	if resp is i64 {
		return resp
	}
	return ProtocolError{ message: '`lpos()`: unexpected response type' }
}

// blpop waits up to timeout seconds for an element from the start of the first nonempty list.
// A zero timeout waits indefinitely; an elapsed timeout returns NilError.
pub fn (mut db DB) blpop(keys []string, timeout f64) !ListPop {
	mut args := ['BLPOP']
	args << keys
	args << timeout.str()
	return db.execute_list_pop(args, false)
}

// brpop waits up to timeout seconds for an element from the end of the first nonempty list.
pub fn (mut db DB) brpop(keys []string, timeout f64) !ListPop {
	mut args := ['BRPOP']
	args << keys
	args << timeout.str()
	return db.execute_list_pop(args, false)
}

// lmove atomically moves an element between the selected ends of two lists.
pub fn (mut db DB) lmove(source string, destination string, from ListSide, to ListSide) !string {
	return db.data_structure_string(['LMOVE', source, destination, from.str().to_upper(),
		to.str().to_upper()])
}

// blmove blocks up to timeout seconds while moving an element between two lists.
pub fn (mut db DB) blmove(source string, destination string, from ListSide, to ListSide, timeout f64) !string {
	return db.data_structure_string(['BLMOVE', source, destination, from.str().to_upper(),
		to.str().to_upper(), timeout.str()])
}

// lmpop removes up to count elements from the first nonempty list, or returns NilError.
pub fn (mut db DB) lmpop(keys []string, side ListSide, count int) !ListPop {
	return db.execute_list_pop(list_multi_pop_args('LMPOP', keys, side, count)!, true)
}

// blmpop blocks up to timeout seconds while removing elements from the first nonempty list.
pub fn (mut db DB) blmpop(timeout f64, keys []string, side ListSide, count int) !ListPop {
	pop_args := list_multi_pop_args('BLMPOP', keys, side, count)!
	mut args := ['BLMPOP', timeout.str()]
	args << pop_args[1..]
	return db.execute_list_pop(args, true)
}

// sadd adds members to a set and returns the number of newly added members.
pub fn (mut db DB) sadd(key string, members ...string) !i64 {
	return db.execute_i64(data_structure_args('SADD', key, members))
}

// srem removes members from a set and returns the number removed.
pub fn (mut db DB) srem(key string, members ...string) !i64 {
	return db.execute_i64(data_structure_args('SREM', key, members))
}

// smembers returns the unordered members of a set under either RESP version.
pub fn (mut db DB) smembers(key string) ![]string {
	return db.execute_set(['SMEMBERS', key])
}

// sismember reports whether a member belongs to a set.
pub fn (mut db DB) sismember(key string, member string) !bool {
	return db.execute_i64(['SISMEMBER', key, member])! != 0
}

// smismember reports membership for each member in input order.
pub fn (mut db DB) smismember(key string, members ...string) ![]bool {
	resp := db.cmd(...data_structure_args('SMISMEMBER', key, members))!
	if db.pipeline_mode || db.transaction_mode {
		return []bool{}
	}
	mut result := []bool{}
	for value in array_value(resp, 'smismember')! {
		if value is i64 {
			result << value != 0
		} else {
			return ProtocolError{ message: '`smismember()`: unexpected response type' }
		}
	}
	return result
}

// scard returns the number of members in a set.
pub fn (mut db DB) scard(key string) !i64 {
	return db.execute_i64(['SCARD', key])
}

// spop removes a random member, or returns NilError for an empty or missing set.
pub fn (mut db DB) spop(key string) !string {
	return db.data_structure_string(['SPOP', key])
}

// spop_count removes up to count random members, returning an empty array if missing.
pub fn (mut db DB) spop_count(key string, count int) ![]string {
	return db.execute_pop_count(['SPOP', key, count.str()])
}

// srandmember returns a random member without removing it, or NilError if missing.
pub fn (mut db DB) srandmember(key string) !string {
	return db.data_structure_string(['SRANDMEMBER', key])
}

// srandmember_count returns count random members; negative counts permit repeated members.
pub fn (mut db DB) srandmember_count(key string, count int) ![]string {
	return db.execute_pop_count(['SRANDMEMBER', key, count.str()])
}

// smove moves a member between sets and reports whether it was present in the source.
pub fn (mut db DB) smove(source string, destination string, member string) !bool {
	return db.execute_i64(['SMOVE', source, destination, member])! != 0
}

fn set_operation_args(command string, keys []string) []string {
	mut args := [command]
	args << keys
	return args
}

// sinter returns the intersection of sets.
pub fn (mut db DB) sinter(keys ...string) ![]string {
	return db.execute_set(set_operation_args('SINTER', keys))
}

// sinterstore replaces destination with the intersection and returns its cardinality.
pub fn (mut db DB) sinterstore(destination string, keys ...string) !i64 {
	return db.execute_i64(data_structure_args('SINTERSTORE', destination, keys))
}

// sintercard counts the intersection, optionally stopping at a positive limit.
pub fn (mut db DB) sintercard(keys []string, limit int) !i64 {
	mut args := ['SINTERCARD', keys.len.str()]
	args << keys
	args << ['LIMIT', limit.str()]
	return db.execute_i64(args)
}

// sunion returns the union of sets.
pub fn (mut db DB) sunion(keys ...string) ![]string {
	return db.execute_set(set_operation_args('SUNION', keys))
}

// sunionstore replaces destination with the union and returns its cardinality.
pub fn (mut db DB) sunionstore(destination string, keys ...string) !i64 {
	return db.execute_i64(data_structure_args('SUNIONSTORE', destination, keys))
}

// sdiff returns members of the first set that occur in none of the remaining sets.
pub fn (mut db DB) sdiff(keys ...string) ![]string {
	return db.execute_set(set_operation_args('SDIFF', keys))
}

// sdiffstore replaces destination with the set difference and returns its cardinality.
pub fn (mut db DB) sdiffstore(destination string, keys ...string) !i64 {
	return db.execute_i64(data_structure_args('SDIFFSTORE', destination, keys))
}

// sscan returns the next cursor and a batch of set members; continue until cursor is '0'.
pub fn (mut db DB) sscan(key string, cursor string, options ScanOptions) !(string, []string) {
	return db.execute_scan(scan_args(['SSCAN', key, cursor], options)!)
}

fn score_value(value RedisValue, command string) !f64 {
	match value {
		f64 { return value }
		i64 { return f64(value) }
		else { return bulk_value[string](value, command)!.f64() }
	}
}

fn sorted_set_values(resp RedisValue, command string) ![]ZMember {
	values := array_value(resp, command)!
	mut result := []ZMember{cap: values.len / 2}
	if values.len == 0 {
		return result
	}
	if values[0] is []RedisValue {
		for value in values {
			pair := array_value(value, command)!
			if pair.len != 2 {
				return ProtocolError{ message: '`${command}()`: invalid member/score pair' }
			}
			result << ZMember{ member: bulk_value[string](pair[0], command)!, score: score_value(pair[1], command)! }
		}
	} else {
		if values.len % 2 != 0 {
			return ProtocolError{ message: '`${command}()`: invalid member/score response' }
		}
		for i := 0; i < values.len; i += 2 {
			result << ZMember{ member: bulk_value[string](values[i], command)!, score: score_value(values[i + 1], command)! }
		}
	}
	return result
}

fn (mut db DB) execute_sorted_set(args []string) ![]ZMember {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode {
		return []ZMember{}
	}
	return sorted_set_values(resp, args[0].to_lower())
}

fn zadd_args(key string, options ZAddOptions, members []ZMember) ![]string {
	if (options.nx && options.xx) || (options.gt && options.lt) || (options.nx && (options.gt || options.lt)) {
		return error('`zadd()`: incompatible NX, XX, GT, or LT options')
	}
	mut args := ['ZADD', key]
	if options.nx { args << 'NX' }
	if options.xx { args << 'XX' }
	if options.gt { args << 'GT' }
	if options.lt { args << 'LT' }
	if options.changed { args << 'CH' }
	for member in members {
		args << [member.score.str(), member.member]
	}
	return args
}

// zadd adds or updates sorted set members and returns the number of newly added members.
pub fn (mut db DB) zadd(key string, members ...ZMember) !i64 {
	return db.execute_i64(zadd_args(key, ZAddOptions{}, members)!)
}

// zadd_with_options adds or updates members using conditional and change-counting options.
pub fn (mut db DB) zadd_with_options(key string, options ZAddOptions, members ...ZMember) !i64 {
	return db.execute_i64(zadd_args(key, options, members)!)
}

// zrem removes sorted set members and returns the number removed.
pub fn (mut db DB) zrem(key string, members ...string) !i64 {
	return db.execute_i64(data_structure_args('ZREM', key, members))
}

// zcard returns the number of sorted set members.
pub fn (mut db DB) zcard(key string) !i64 {
	return db.execute_i64(['ZCARD', key])
}

// zcount counts scores within bounds, supporting infinity and exclusive '(value' bounds.
pub fn (mut db DB) zcount(key string, min string, max string) !i64 {
	return db.execute_i64(['ZCOUNT', key, min, max])
}

// zscore returns a member's score, or NilError if the member is missing.
pub fn (mut db DB) zscore(key string, member string) !f64 {
	resp := db.cmd('ZSCORE', key, member)!
	if db.pipeline_mode || db.transaction_mode { return 0.0 }
	if resp is RedisNull { return NilError{ message: '`zscore()`: member not found' } }
	return score_value(resp, 'zscore')
}

// zmscore returns scores in input order, preserving missing members as none.
pub fn (mut db DB) zmscore(key string, members ...string) ![]?f64 {
	resp := db.cmd(...data_structure_args('ZMSCORE', key, members))!
	if db.pipeline_mode || db.transaction_mode { return []?f64{} }
	mut result := []?f64{}
	for value in array_value(resp, 'zmscore')! {
		if value is RedisNull {
			result << none
		} else {
			result << ?f64(score_value(value, 'zmscore')!)
		}
	}
	return result
}

// zincrby increments a member's score, creating the member if necessary.
pub fn (mut db DB) zincrby(key string, increment f64, member string) !f64 {
	return db.execute_f64(['ZINCRBY', key, increment.str(), member])
}

fn (mut db DB) execute_rank(args []string) !i64 {
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode { return 0 }
	if resp is RedisNull {
		return NilError{ message: '`${args[0].to_lower()}()`: member not found' }
	}
	if resp is i64 { return resp }
	return ProtocolError{ message: '`${args[0].to_lower()}()`: unexpected response type' }
}

// zrank returns a member's zero-based ascending rank, or NilError if missing.
pub fn (mut db DB) zrank(key string, member string) !i64 {
	return db.execute_rank(['ZRANK', key, member])
}

// zrevrank returns a member's zero-based descending rank, or NilError if missing.
pub fn (mut db DB) zrevrank(key string, member string) !i64 {
	return db.execute_rank(['ZREVRANK', key, member])
}

fn zrange_args(key string, start i64, stop i64, options ZRangeOptions, with_scores bool) []string {
	mut args := ['ZRANGE', key, start.str(), stop.str()]
	if options.rev { args << 'REV' }
	if with_scores { args << 'WITHSCORES' }
	return args
}

// zrange returns members at inclusive rank offsets, optionally in reverse order.
pub fn (mut db DB) zrange(key string, start i64, stop i64, options ZRangeOptions) ![]string {
	return db.execute_strings(zrange_args(key, start, stop, options, false))
}

// zrange_withscores returns members and scores at inclusive rank offsets.
pub fn (mut db DB) zrange_withscores(key string, start i64, stop i64, options ZRangeOptions) ![]ZMember {
	return db.execute_sorted_set(zrange_args(key, start, stop, options, true))
}

// zrevrange returns members at inclusive rank offsets in descending order.
pub fn (mut db DB) zrevrange(key string, start i64, stop i64) ![]string {
	return db.execute_strings(['ZREVRANGE', key, start.str(), stop.str()])
}

// zrevrange_withscores returns members and scores in descending rank order.
pub fn (mut db DB) zrevrange_withscores(key string, start i64, stop i64) ![]ZMember {
	return db.execute_sorted_set(['ZREVRANGE', key, start.str(), stop.str(), 'WITHSCORES'])
}

fn zrange_limit_args(args []string, options ZRangeLimit) ![]string {
	mut result := args.clone()
	if count := options.count {
		if options.offset < 0 {
			return error('`${args[0].to_lower()}()`: offset must not be negative')
		}
		result << ['LIMIT', options.offset.str(), count.str()]
	} else if options.offset != 0 {
		return error('`${args[0].to_lower()}()`: offset requires count')
	}
	return result
}

// zrangebyscore returns members within score bounds, with optional offset and count.
pub fn (mut db DB) zrangebyscore(key string, min string, max string, options ZRangeLimit) ![]string {
	return db.execute_strings(zrange_limit_args(['ZRANGEBYSCORE', key, min, max], options)!)
}

// zrangebyscore_withscores returns members and scores within score bounds.
pub fn (mut db DB) zrangebyscore_withscores(key string, min string, max string, options ZRangeLimit) ![]ZMember {
	mut args := zrange_limit_args(['ZRANGEBYSCORE', key, min, max], options)!
	args << 'WITHSCORES'
	return db.execute_sorted_set(args)
}

// zrangebylex returns members within lexicographic bounds for a set whose scores are equal.
pub fn (mut db DB) zrangebylex(key string, min string, max string, options ZRangeLimit) ![]string {
	return db.execute_strings(zrange_limit_args(['ZRANGEBYLEX', key, min, max], options)!)
}

fn zpop_args(command string, key string, count []int) ![]string {
	if count.len > 1 { return error('`${command.to_lower()}()`: at most one count is allowed') }
	mut args := [command, key]
	if count.len == 1 { args << count[0].str() }
	return args
}

// zpopmax removes the highest scoring members; omitted count defaults to one.
pub fn (mut db DB) zpopmax(key string, count ...int) ![]ZMember {
	return db.execute_sorted_set(zpop_args('ZPOPMAX', key, count)!)
}

// zpopmin removes the lowest scoring members; omitted count defaults to one.
pub fn (mut db DB) zpopmin(key string, count ...int) ![]ZMember {
	return db.execute_sorted_set(zpop_args('ZPOPMIN', key, count)!)
}

fn (mut db DB) execute_blocking_zpop(command string, timeout f64, keys []string) !ZPop {
	mut args := [command]
	args << keys
	args << timeout.str()
	resp := db.cmd(...args)!
	if db.pipeline_mode || db.transaction_mode { return ZPop{} }
	if resp is RedisNull {
		return NilError{ message: '`${command.to_lower()}()`: no member available' }
	}
	values := array_value(resp, command.to_lower())!
	if values.len != 3 {
		return ProtocolError{ message: '`${command.to_lower()}()`: invalid blocking pop response' }
	}
	return ZPop{ key: bulk_value[string](values[0], command)!, value: ZMember{ member: bulk_value[string](values[1], command)!, score: score_value(values[2], command)! } }
}

// bzpopmax blocks up to timeout seconds for the highest scoring member of the first nonempty set.
pub fn (mut db DB) bzpopmax(timeout f64, keys ...string) !ZPop {
	return db.execute_blocking_zpop('BZPOPMAX', timeout, keys)
}

// bzpopmin blocks up to timeout seconds for the lowest scoring member of the first nonempty set.
pub fn (mut db DB) bzpopmin(timeout f64, keys ...string) !ZPop {
	return db.execute_blocking_zpop('BZPOPMIN', timeout, keys)
}

// zrandmember returns a random member without removing it, or NilError if missing.
pub fn (mut db DB) zrandmember(key string) !string {
	return db.data_structure_string(['ZRANDMEMBER', key])
}

// zrandmember_count returns random members; negative counts permit repeated members.
pub fn (mut db DB) zrandmember_count(key string, count int) ![]string {
	return db.execute_strings(['ZRANDMEMBER', key, count.str()])
}

// zrandmember_withscores returns random members with scores, permitting repeats for negative count.
pub fn (mut db DB) zrandmember_withscores(key string, count int) ![]ZMember {
	return db.execute_sorted_set(['ZRANDMEMBER', key, count.str(), 'WITHSCORES'])
}

fn zdiff_args(command string, destination string, keys []string, with_scores bool) []string {
	mut args := [command]
	if command == 'ZDIFFSTORE' { args << destination }
	args << keys.len.str()
	args << keys
	if with_scores { args << 'WITHSCORES' }
	return args
}

// zdiff returns the sorted set difference of the given keys.
pub fn (mut db DB) zdiff(keys ...string) ![]string {
	return db.execute_strings(zdiff_args('ZDIFF', '', keys, false))
}

// zdiff_withscores returns the sorted set difference with scores from the first set.
pub fn (mut db DB) zdiff_withscores(keys ...string) ![]ZMember {
	return db.execute_sorted_set(zdiff_args('ZDIFF', '', keys, true))
}

// zdiffstore replaces destination with the difference and returns its cardinality.
pub fn (mut db DB) zdiffstore(destination string, keys ...string) !i64 {
	return db.execute_i64(zdiff_args('ZDIFFSTORE', destination, keys, false))
}

fn zcombine_args(command string, destination string, keys []WeightedKey, options ZCombineOptions, with_scores bool) []string {
	mut args := [command]
	if command.ends_with('STORE') { args << destination }
	args << keys.len.str()
	for key in keys { args << key.key }
	args << 'WEIGHTS'
	for key in keys { args << key.weight.str() }
	args << ['AGGREGATE', options.aggregate.str().to_upper()]
	if with_scores { args << 'WITHSCORES' }
	return args
}

// zinter returns the weighted intersection of sorted sets.
pub fn (mut db DB) zinter(keys []WeightedKey, options ZCombineOptions) ![]string {
	return db.execute_strings(zcombine_args('ZINTER', '', keys, options, false))
}

// zinter_withscores returns members and aggregate scores in a weighted intersection.
pub fn (mut db DB) zinter_withscores(keys []WeightedKey, options ZCombineOptions) ![]ZMember {
	return db.execute_sorted_set(zcombine_args('ZINTER', '', keys, options, true))
}

// zinterstore replaces destination with a weighted intersection and returns its cardinality.
pub fn (mut db DB) zinterstore(destination string, keys []WeightedKey, options ZCombineOptions) !i64 {
	return db.execute_i64(zcombine_args('ZINTERSTORE', destination, keys, options, false))
}

// zunion returns the weighted union of sorted sets.
pub fn (mut db DB) zunion(keys []WeightedKey, options ZCombineOptions) ![]string {
	return db.execute_strings(zcombine_args('ZUNION', '', keys, options, false))
}

// zunion_withscores returns members and aggregate scores in a weighted union.
pub fn (mut db DB) zunion_withscores(keys []WeightedKey, options ZCombineOptions) ![]ZMember {
	return db.execute_sorted_set(zcombine_args('ZUNION', '', keys, options, true))
}

// zunionstore replaces destination with a weighted union and returns its cardinality.
pub fn (mut db DB) zunionstore(destination string, keys []WeightedKey, options ZCombineOptions) !i64 {
	return db.execute_i64(zcombine_args('ZUNIONSTORE', destination, keys, options, false))
}

// zintercard counts the intersection, optionally stopping at a positive limit.
pub fn (mut db DB) zintercard(keys []string, limit int) !i64 {
	mut args := ['ZINTERCARD', keys.len.str()]
	args << keys
	args << ['LIMIT', limit.str()]
	return db.execute_i64(args)
}

// zlexcount counts members within lexicographic bounds for a set whose scores are equal.
pub fn (mut db DB) zlexcount(key string, min string, max string) !i64 {
	return db.execute_i64(['ZLEXCOUNT', key, min, max])
}

// zremrangebyrank removes members at inclusive rank offsets.
pub fn (mut db DB) zremrangebyrank(key string, start i64, stop i64) !i64 {
	return db.execute_i64(['ZREMRANGEBYRANK', key, start.str(), stop.str()])
}

// zremrangebyscore removes members within score bounds.
pub fn (mut db DB) zremrangebyscore(key string, min string, max string) !i64 {
	return db.execute_i64(['ZREMRANGEBYSCORE', key, min, max])
}

// zremrangebylex removes members within lexicographic bounds.
pub fn (mut db DB) zremrangebylex(key string, min string, max string) !i64 {
	return db.execute_i64(['ZREMRANGEBYLEX', key, min, max])
}

// zscan returns the next cursor and a batch of members and scores; continue until cursor is '0'.
pub fn (mut db DB) zscan(key string, cursor string, options ScanOptions) !(string, []ZMember) {
	resp := db.cmd(...scan_args(['ZSCAN', key, cursor], options)!)!
	if db.pipeline_mode || db.transaction_mode { return '', []ZMember{} }
	values := array_value(resp, 'zscan')!
	if values.len != 2 { return ProtocolError{ message: '`zscan()`: invalid scan response' } }
	return bulk_value[string](values[0], 'zscan')!, sorted_set_values(values[1], 'zscan')!
}
