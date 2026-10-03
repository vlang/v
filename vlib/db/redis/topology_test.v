module redis

import net
import time

struct TopologyStep {
	args       []string
	reply      string
	disconnect bool
}

struct TopologySession {
	steps []TopologyStep
}

fn topology_server(mut listener net.TcpListener, sessions []TopologySession) ! {
	defer { listener.close() or {} }
	for session in sessions {
		mut conn := listener.accept()!
		conn.set_read_timeout(2 * time.second)
		mut db := DB{ version: 3, conn: conn }
		for step in session.steps {
			actual := string_values(db.read_response()!, 'mock server')!
			if actual != step.args {
				db.close() or {}
				return error('expected ${step.args}, received ${actual}')
			}
			if step.disconnect { break }
			conn.write_string(step.reply)!
		}
		db.close() or {}
	}
}

fn topology_listener() !&net.TcpListener {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	listener.set_accept_timeout(2 * time.second)
	return listener
}

fn topology_bulk(value string) string {
	return '$${value.len}\r\n${value}\r\n'
}

fn topology_master_address(port u16) string {
	return '*2\r\n' + topology_bulk('127.0.0.1') + topology_bulk(port.str())
}

fn topology_slots(port u16) string {
	return '*1\r\n*3\r\n:0\r\n:16383\r\n*2\r\n' + topology_bulk('127.0.0.1') + ':${port}\r\n'
}

fn topology_config(port u16) Config {
	return Config{ host: '127.0.0.1', port: port, read_timeout: time.second, write_timeout: time.second }
}

fn topology_hello() TopologyStep {
	return TopologyStep{ args: ['HELLO', '3'], reply: '+OK\r\n' }
}

fn topology_role() TopologyStep {
	return TopologyStep{ args: ['ROLE'], reply: '*3\r\n$6\r\nmaster\r\n:0\r\n*0\r\n' }
}

fn test_cluster_hash_tag_vectors() {
	// CRC16/XMODEM check value 0x31c3, reduced modulo the 16384 cluster slots.
	assert hash_slot('123456789') == 12739
	assert hash_slot('foo') == 12182
	assert hash_slot('bar') == 5061
	assert hash_slot('foo{bar}{zap}') == 5061
	assert hash_slot('{user}:one') == 5474
	assert hash_slot('{user}:two') == 5474
	// An empty first tag or an unmatched opening brace hashes the entire key.
	assert hash_slot('foo{}{bar}') == 8363
	assert hash_slot('a{') == 14311
	assert hash_slot('{}') == 15257
	assert hash_slot('') == 0
}

fn test_cluster_moved_updates_route_and_reuses_connection() {
	mut seed := topology_listener()!
	mut target := topology_listener()!
	seed_port := seed.addr()!.port()!
	target_port := target.addr()!.port()!
	seed_worker := spawn topology_server(mut seed, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['CLUSTER', 'SLOTS'], reply: topology_slots(seed_port) },
			TopologyStep{ args: ['GET', 'foo'], reply: '-MOVED 12182 127.0.0.1:${target_port}\r\n' },
		]
	}])
	target_worker := spawn topology_server(mut target, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['GET', 'foo'], reply: topology_bulk('moved') },
			TopologyStep{ args: ['GET', 'foo'], reply: topology_bulk('cached') },
			TopologyStep{ args: ['SET', 'foo', 'value'], reply: '+OK\r\n' },
		]
	}])
	mut cluster := connect_cluster([topology_config(seed_port)])!
	defer { cluster.close() or {} }
	assert cluster.get[string]('foo')! == 'moved'
	assert cluster.slots[12182] == '127.0.0.1:${target_port}'
	assert cluster.get[string]('foo')! == 'cached'
	assert cluster.set('foo', 'value')! == 'OK'
	assert cluster.nodes.len == 2
	seed_worker.wait()!
	target_worker.wait()!
}

fn test_cluster_ask_sends_asking_without_changing_route() {
	mut seed := topology_listener()!
	mut target := topology_listener()!
	seed_port := seed.addr()!.port()!
	target_port := target.addr()!.port()!
	seed_worker := spawn topology_server(mut seed, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['CLUSTER', 'SLOTS'], reply: topology_slots(seed_port) },
			TopologyStep{ args: ['GET', 'foo'], reply: '-ASK 12182 127.0.0.1:${target_port}\r\n' },
			TopologyStep{ args: ['GET', 'foo'], reply: topology_bulk('original') },
		]
	}])
	target_worker := spawn topology_server(mut target, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['ASKING'], reply: '+OK\r\n' },
			TopologyStep{ args: ['GET', 'foo'], reply: topology_bulk('temporary') },
		]
	}])
	mut cluster := connect_cluster([topology_config(seed_port)])!
	defer { cluster.close() or {} }
	assert cluster.get[string]('foo')! == 'temporary'
	assert cluster.slots[12182] == '127.0.0.1:${seed_port}'
	assert cluster.get[string]('foo')! == 'original'
	seed_worker.wait()!
	target_worker.wait()!
}

fn test_cluster_slot_discovery_rejects_invalid_ranges() {
	mut seed := topology_listener()!
	port := seed.addr()!.port()!
	worker := spawn topology_server(mut seed, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['CLUSTER', 'SLOTS'], reply: '*1\r\n*3\r\n:0\r\n:16384\r\n*2\r\n$9\r\n127.0.0.1\r\n:6379\r\n' },
		]
	}])
	if mut cluster := connect_cluster([topology_config(port)]) {
		cluster.close() or {}
		assert false, 'invalid slot range accepted'
	} else {
		assert err is ConnectionError
		assert err.msg().contains('invalid cluster slot range')
	}
	worker.wait()!
}

fn test_cluster_rejects_a_nonnumeric_redirect_slot() {
	mut seed := topology_listener()!
	port := seed.addr()!.port()!
	worker := spawn topology_server(mut seed, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['CLUSTER', 'SLOTS'], reply: topology_slots(port) },
			TopologyStep{ args: ['GET', 'foo'], reply: '-MOVED invalid 127.0.0.1:${port}\r\n' },
		]
	}])
	mut cluster := connect_cluster([topology_config(port)])!
	defer { cluster.close() or {} }
	if value := cluster.get[string]('foo') {
		assert false, 'invalid redirect was accepted: ${value}'
	} else {
		assert err is ProtocolError
	}
	worker.wait()!
}

fn test_cluster_discovers_slots_from_the_next_available_seed() {
	mut first := topology_listener()!
	mut second := topology_listener()!
	first_port := first.addr()!.port()!
	second_port := second.addr()!.port()!
	first_worker := spawn topology_server(mut first, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['CLUSTER', 'SLOTS'], reply: '-ERR cluster discovery unavailable\r\n' },
		]
	}])
	second_worker := spawn topology_server(mut second, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['CLUSTER', 'SLOTS'], reply: topology_slots(second_port) },
			TopologyStep{ args: ['GET', 'foo'], reply: topology_bulk('second seed') },
		]
	}])
	mut cluster := connect_cluster([topology_config(first_port), topology_config(second_port)])!
	defer { cluster.close() or {} }
	assert cluster.get[string]('foo')! == 'second seed'
	first_worker.wait()!
	second_worker.wait()!
}

fn test_sentinel_readonly_rediscovery_verifies_master_role() {
	mut sentinel := topology_listener()!
	mut old_master := topology_listener()!
	mut new_master := topology_listener()!
	sentinel_port := sentinel.addr()!.port()!
	old_port := old_master.addr()!.port()!
	new_port := new_master.addr()!.port()!
	sentinel_worker := spawn topology_server(mut sentinel, [
		TopologySession{
			steps: [topology_hello(),
				TopologyStep{ args: ['SENTINEL', 'GET-MASTER-ADDR-BY-NAME', 'test-master'], reply: topology_master_address(old_port) }]
		},
		TopologySession{
			steps: [topology_hello(),
				TopologyStep{ args: ['SENTINEL', 'GET-MASTER-ADDR-BY-NAME', 'test-master'], reply: topology_master_address(new_port) }]
		},
	])
	old_worker := spawn topology_server(mut old_master, [TopologySession{
		steps: [
			topology_hello(),
			topology_role(),
			TopologyStep{ args: ['SET', 'key', 'value'], reply: '-READONLY You cannot write against a read only replica.\r\n' },
		]
	}])
	new_worker := spawn topology_server(mut new_master, [TopologySession{
		steps: [
			topology_hello(),
			topology_role(),
			TopologyStep{ args: ['SET', 'key', 'value'], reply: '+OK\r\n' },
			TopologyStep{ args: ['GET', 'key'], reply: topology_bulk('value') },
		]
	}])
	mut client := connect_sentinel(
		sentinels:     [topology_config(sentinel_port)]
		master_name:   'test-master'
		master_config: topology_config(6379)
	)!
	defer { client.close() or {} }
	assert client.cmd('SET', 'key', 'value')! as string == 'OK'
	assert (client.cmd('GET', 'key')! as []u8).bytestr() == 'value'
	sentinel_worker.wait()!
	old_worker.wait()!
	new_worker.wait()!
}

fn test_sentinel_transport_failure_does_not_replay_a_write() {
	mut sentinel := topology_listener()!
	mut old_master := topology_listener()!
	mut new_master := topology_listener()!
	sentinel_port := sentinel.addr()!.port()!
	old_port := old_master.addr()!.port()!
	new_port := new_master.addr()!.port()!
	sentinel_worker := spawn topology_server(mut sentinel, [
		TopologySession{
			steps: [topology_hello(),
				TopologyStep{ args: ['SENTINEL', 'GET-MASTER-ADDR-BY-NAME', 'test-master'], reply: topology_master_address(old_port) }]
		},
		TopologySession{
			steps: [topology_hello(),
				TopologyStep{ args: ['SENTINEL', 'GET-MASTER-ADDR-BY-NAME', 'test-master'], reply: topology_master_address(new_port) }]
		},
	])
	old_worker := spawn topology_server(mut old_master, [TopologySession{
		steps: [
			topology_hello(),
			topology_role(),
			TopologyStep{ args: ['INCR', 'counter'], disconnect: true },
		]
	}])
	new_worker := spawn topology_server(mut new_master, [TopologySession{
		steps: [
			topology_hello(),
			topology_role(),
			TopologyStep{ args: ['GET', 'counter'], reply: topology_bulk('1') },
		]
	}])
	mut client := connect_sentinel(
		sentinels:     [topology_config(sentinel_port)]
		master_name:   'test-master'
		master_config: topology_config(6379)
	)!
	defer { client.close() or {} }
	if value := client.cmd('INCR', 'counter') {
		assert false, 'transport failure was hidden: ${value}'
	} else {
		assert err is ConnectionError
	}
	assert (client.cmd('GET', 'counter')! as []u8).bytestr() == '1'
	sentinel_worker.wait()!
	old_worker.wait()!
	new_worker.wait()!
}

fn test_sentinel_skips_a_discovered_replica() {
	mut first_sentinel := topology_listener()!
	mut second_sentinel := topology_listener()!
	mut replica := topology_listener()!
	mut master := topology_listener()!
	first_port := first_sentinel.addr()!.port()!
	second_port := second_sentinel.addr()!.port()!
	replica_port := replica.addr()!.port()!
	master_port := master.addr()!.port()!
	first_worker := spawn topology_server(mut first_sentinel, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['SENTINEL', 'GET-MASTER-ADDR-BY-NAME', 'test-master'], reply: topology_master_address(replica_port) },
		]
	}])
	second_worker := spawn topology_server(mut second_sentinel, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['SENTINEL', 'GET-MASTER-ADDR-BY-NAME', 'test-master'], reply: topology_master_address(master_port) },
		]
	}])
	replica_worker := spawn topology_server(mut replica, [TopologySession{
		steps: [
			topology_hello(),
			TopologyStep{ args: ['ROLE'], reply: '*1\r\n$5\r\nslave\r\n' },
		]
	}])
	master_worker := spawn topology_server(mut master, [TopologySession{
		steps: [
			topology_hello(),
			topology_role(),
			TopologyStep{ args: ['PING'], reply: '+PONG\r\n' },
		]
	}])
	mut client := connect_sentinel(
		sentinels:     [topology_config(first_port), topology_config(second_port)]
		master_name:   'test-master'
		master_config: topology_config(6379)
	)!
	defer { client.close() or {} }
	assert client.cmd('PING')! as string == 'PONG'
	first_worker.wait()!
	second_worker.wait()!
	replica_worker.wait()!
	master_worker.wait()!
}
