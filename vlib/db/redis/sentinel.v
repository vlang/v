module redis

import strconv

// SentinelConfig separates Sentinel credentials from the discovered master's connection settings.
@[params]
pub struct SentinelConfig {
pub:
	sentinels     []Config
	master_name   string
	master_config Config
}

// Sentinel owns a discovered master connection and can rediscover it after failover.
pub struct Sentinel {
mut:
	config SentinelConfig
	db     DB
}

fn discover_master(config SentinelConfig) !DB {
	if config.master_name == '' || config.sentinels.len == 0 {
		return ConnectionError{ message: 'Sentinel requires endpoints and a master name' }
	}
	mut failure := 'no reachable Sentinel'
	for seed in config.sentinels {
		mut sentinel := connect(seed) or {
			failure = err.msg()
			continue
		}
		defer { sentinel.close() or {} }
		response := sentinel.cmd('SENTINEL', 'GET-MASTER-ADDR-BY-NAME', config.master_name) or {
			failure = err.msg()
			continue
		}
		if response is RedisNull {
			failure = 'unknown Sentinel master'
			continue
		}
		address := string_values(response, 'sentinel')!
		if address.len != 2 { return ProtocolError{ message: 'invalid Sentinel master address' } }
		port := strconv.parse_int(address[1], 10, 32) or { return ProtocolError{ message: 'invalid Sentinel master port' } }
		if port <= 0 || port > 65535 {
			return ProtocolError{ message: 'invalid Sentinel master port' }
		}
		mut master := connect(Config{ ...config.master_config, host: address[0], port: u16(port) }) or {
			failure = err.msg()
			continue
		}
		role := master.cmd('ROLE') or {
			master.close() or {}
			failure = err.msg()
			continue
		}
		values := array_value(role, 'role')!
		if values.len == 0 || bulk_value[string](values[0], 'role')! != 'master' {
			master.close() or {}
			failure = 'Sentinel returned a node that is not master'
			continue
		}
		return master
	}
	return ConnectionError{ message: failure }
}

// connect_sentinel discovers a master and verifies its ROLE before exposing it.
pub fn connect_sentinel(config SentinelConfig) !Sentinel {
	return Sentinel{ config: config, db: discover_master(config)! }
}

// reconnect reconsults Sentinel and replaces the connection to the current master.
pub fn (mut client Sentinel) reconnect() ! {
	client.db.close() or {}
	client.db = discover_master(client.config)!
}

// cmd executes on the discovered master. READONLY replies trigger discovery and one safe retry.
// Transport failures rediscover for the next call, but the failed command is not replayed.
pub fn (mut client Sentinel) cmd(args ...string) !RedisValue {
	return client.db.cmd(...args) or {
		if err is CommandError && err.msg().starts_with('READONLY') {
			client.reconnect()!
			return client.db.cmd(...args)
		}
		if err is ConnectionError { client.reconnect() or {} }
		return err
	}
}

// close closes the current master connection.
pub fn (mut client Sentinel) close() ! {
	client.db.close()!
}
