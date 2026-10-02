module redis

import strconv

// Cluster routes commands by hash slot and follows MOVED and ASK redirects.
// One Cluster belongs to one caller; use separate clients for concurrent requests.
pub struct Cluster {
mut:
	config Config
	slots  []string
	nodes  map[string]&DB
}

// hash_slot returns Redis's CRC16 slot, honoring the first nonempty {...} hash tag.
pub fn hash_slot(key string) int {
	mut data := key
	start := key.index('{') or { -1 }
	if start >= 0 {
		end := key[start + 1..].index('}') or { -1 }
		if end > 0 { data = key[start + 1..start + 1 + end] }
	}
	mut crc := u16(0)
	for b in data.bytes() {
		crc ^= u16(b) << 8
		for _ in 0 .. 8 {
			crc = if crc & 0x8000 != 0 { (crc << 1) ^ u16(0x1021) } else { crc << 1 }
		}
	}
	return int(crc & 0x3fff)
}

fn endpoint(config Config) string {
	if config.host.contains(':') && !config.host.starts_with('[') {
		return '[${config.host}]:${config.port}'
	}
	return '${config.host}:${config.port}'
}

fn endpoint_config(address string, fallback Config) !Config {
	separator := address.last_index(':') or { return ProtocolError{ message: 'invalid Redis endpoint' } }
	host := address[..separator].trim('[]')
	port := strconv.parse_int(address[separator + 1..], 10, 32) or { return ProtocolError{ message: 'invalid Redis endpoint port' } }
	if port <= 0 || port > 65535 { return ProtocolError{ message: 'invalid Redis endpoint port' } }
	return Config{ ...fallback, host: if host == '' { fallback.host } else { host }, port: u16(port), database: 0 }
}

// connect_cluster discovers the slot map from the first available seed.
// Config settings on that seed are reused for discovered nodes; clusters only support database zero.
pub fn connect_cluster(seeds []Config) !Cluster {
	if seeds.len == 0 { return ConnectionError{ message: 'cluster requires at least one seed' } }
	mut last_error := 'no reachable cluster seed'
	for seed in seeds {
		if seed.database != 0 {
			return CommandError{ message: 'clusters only support database zero' }
		}
		mut db := connect(seed) or {
			last_error = err.msg()
			continue
		}
		mut cluster := Cluster{
			config: seed
			slots:  []string{len: 16384}
			nodes:  {
				endpoint(seed): &db
			}
		}
		cluster.refresh_slots() or {
			cluster.close() or {}
			last_error = err.msg()
			continue
		}
		return cluster
	}
	return ConnectionError{ message: last_error }
}

// refresh_slots atomically replaces the slot map using CLUSTER SLOTS.
pub fn (mut cluster Cluster) refresh_slots() ! {
	mut last_error := 'no reachable cluster node'
	for _, mut db in cluster.nodes {
		response := db.cmd('CLUSTER', 'SLOTS') or {
			last_error = err.msg()
			continue
		}
		mut slots := []string{len: 16384}
		for item in array_value(response, 'cluster slots')! {
			range := array_value(item, 'cluster slots')!
			if range.len < 3 || range[0] !is i64 || range[1] !is i64 {
				return ProtocolError{ message: 'invalid cluster slot range' }
			}
			start := int(range[0] as i64)
			end := int(range[1] as i64)
			if start < 0 || end < start || end >= 16384 {
				return ProtocolError{ message: 'invalid cluster slot range' }
			}
			node := array_value(range[2], 'cluster slots')!
			if node.len < 2 || node[1] !is i64 {
				return ProtocolError{ message: 'invalid cluster node' }
			}
			host := if node[0] is RedisNull {
				db.config.host
			} else {
				bulk_value[string](node[0], 'cluster slots')!
			}
			port := node[1] as i64
			if port <= 0 || port > 65535 {
				return ProtocolError{ message: 'invalid cluster node port' }
			}
			address := endpoint(Config{ host: if host == '' { db.config.host } else { host }, port: u16(port) })
			for slot in start .. end + 1 { slots[slot] = address }
		}
		cluster.slots = slots
		return
	}
	return ConnectionError{ message: last_error }
}

fn (mut cluster Cluster) node(address string) !&DB {
	if db := cluster.nodes[address] { return db }
	config := endpoint_config(address, cluster.config)!
	mut db := connect(config)!
	cluster.nodes[address] = &db
	return &db
}

// cmd routes a command using key, which must identify the command's Redis hash slot.
// All keys in a multi-key command must share that slot. Server-side redirects are bounded to 16 hops.
pub fn (mut cluster Cluster) cmd(key string, args ...string) !RedisValue {
	if args.len == 0 { return CommandError{ message: 'command must not be empty' } }
	slot := hash_slot(key)
	mut address := cluster.slots[slot]
	if address == '' {
		cluster.refresh_slots()!
		address = cluster.slots[slot]
	}
	if address == '' { return ConnectionError{ message: 'cluster slot is not assigned' } }
	mut asking := false
	for _ in 0 .. 16 {
		mut db := cluster.node(address)!
		if asking { db.cmd('ASKING')! }
		response := db.cmd(...args) or {
			if err !is CommandError { return err }
			parts := err.msg().split(' ')
			if parts.len != 3 || parts[0] !in ['MOVED', 'ASK'] { return err }
			redirect_slot := int(strconv.parse_int(parts[1], 10, 32) or { return ProtocolError{ message: 'invalid cluster redirect slot' } })
			if redirect_slot < 0 || redirect_slot >= 16384 {
				return ProtocolError{ message: 'invalid cluster redirect slot' }
			}
			target := endpoint_config(parts[2], db.config)!
			address = endpoint(target)
			asking = parts[0] == 'ASK'
			if !asking { cluster.slots[redirect_slot] = address }
			continue
		}
		return response
	}
	return CommandError{ message: 'too many cluster redirects' }
}

// set stores a value on its owning cluster node.
pub fn (mut cluster Cluster) set[T](key string, value T) !string {
	return bulk_value[string](cluster.cmd(key, 'SET', key, value_string(value, 'set')!)!, 'set')
}

// get reads a value from its owning cluster node.
pub fn (mut cluster Cluster) get[T](key string) !T {
	return bulk_value[T](cluster.cmd(key, 'GET', key)!, 'get')
}

// close closes every cached cluster connection.
pub fn (mut cluster Cluster) close() ! {
	mut failure := ''
	for _, mut db in cluster.nodes { db.close() or { failure = err.msg() } }
	cluster.nodes.clear()
	if failure != '' { return ConnectionError{ message: failure } }
}
